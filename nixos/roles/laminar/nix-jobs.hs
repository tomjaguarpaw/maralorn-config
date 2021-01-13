{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-missing-signatures -Wno-type-defaults -Wno-orphans #-}

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( forConcurrently_
                                                , race_
                                                , withAsync
                                                )
import           Control.Concurrent.STM         ( check
                                                , retry
                                                )
import           Control.Exception              ( bracket
                                                , catch
                                                , handle
                                                , handleJust
                                                , throwIO
                                                )
import           Data.Bits                      ( Bits((.|.)) )
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as Seq
import           Data.String.Interpolate        ( i )
import           Data.Text                      ( isInfixOf
                                                , splitOn
                                                , strip
                                                )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , diffUTCTime
                                                , formatTime
                                                , getCurrentTime
                                                )
import           Relude
import           Say                            ( say
                                                , sayErr
                                                )
import           Shh                            ( (&!>)
                                                , ExecArg(..)
                                                , ExecReference(Absolute)
                                                , Stream(StdOut)
                                                , captureTrim
                                                , load
                                                , (|>)
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                , getModificationTime
                                                , removeFile
                                                )
import           System.Environment             ( getArgs
                                                , getEnv
                                                , setEnv
                                                )
import           System.FSNotify                ( Event(Removed)
                                                , watchDir
                                                , withManager
                                                )
import           System.IO                      ( BufferMode(LineBuffering)
                                                , hSetBuffering
                                                )
import           System.IO.Error
import           System.IO.Unsafe
import           System.Posix.Files             ( groupReadMode
                                                , otherReadMode
                                                , ownerReadMode
                                                , ownerWriteMode
                                                )
import           System.Posix.IO                ( OpenFileFlags(exclusive)
                                                , OpenMode(WriteOnly)
                                                , closeFd
                                                , defaultFileFlags
                                                , fdWrite
                                                , openFd
                                                )


load Absolute ["laminarc", "nix-store"]

data JobResult = Success | Failure deriving (Show, Read, Eq, Ord, Enum)

newtype JobException = JobException Text deriving (Show, Exception)
throw = throwIO . JobException
newtype WaitException = WaitException Text deriving (Show, Exception)
throwWait = throwIO . WaitException

instance Semigroup JobResult where
  Success <> Success = Success
  _       <> _       = Failure

instance Monoid JobResult where
  mempty = Success

instance ExecArg Text where
  asArg         = asArg . toString
  asArgFromList = asArgFromList . fmap toString

drvBasename :: Text -> Text
drvBasename derivationName =
  fromMaybe derivationName . viaNonEmpty last $ splitOn "/" derivationName

workspace, resultDir, runningDir :: String
workspace = "/var/lib/laminar/run/nix-build/workspace"
resultDir = [i|#{workspace}/completed-jobs|]
runningDir = [i|#{workspace}/running-jobs|]
runningPath, resultPath :: Text -> String
runningPath p = [i|#{runningDir}/#{drvBasename p}|]
resultPath p = [i|#{resultDir}/#{drvBasename p}|]

{-# NOINLINE jobMap #-}

data BuildState = Pending | Running UTCTime | Complete deriving (Show, Eq)
-- True means job is finished
jobMap :: TVar (Map Text (TVar BuildState))
jobMap = unsafePerformIO $ newTVarIO mempty

-- Bool means derivation itself needs to be build
getDependenciesFromNix :: Text -> IO (Seq Text)
getDependenciesFromNix derivationName = do
  everythingToDo <- nixStoreRealiseDryRun derivationName
  pure $ Seq.filter (/= derivationName) everythingToDo

-- Bool means derivation itself needs to be build
needsBuild :: Text -> IO Bool
needsBuild derivationName = do
  everythingToDo <- nixStoreRealiseDryRun derivationName
  let depsToDo = Seq.filter (/= derivationName) everythingToDo
  pure (everythingToDo /= depsToDo)

nixStoreRealiseDryRun :: Text -> IO (Seq Text)
nixStoreRealiseDryRun derivationName = do
  process
    <$> (nix_store "-r" derivationName "--dry-run" &!> StdOut |> captureTrim)
 where
  process =
    fromList
      . drop 1
      . dropWhile (/= "these derivations will be built:")
      . takeWhile (not . T.isPrefixOf "these paths will be fetched")
      . fmap strip
      . lines
      . decodeUtf8

job :: Text -> IO ()
job derivationName = do
  when (T.null derivationName) $ do
    sayErr [i|Empty derivationName. Can’t realise that.|]
    exitFailure
  say [i|Building #{derivationName}.|]
  whenM (not <$> doesFileExist (runningPath derivationName)) $ do
    sayErr
      [i|No file found at #{runningPath derivationName}. Did you start realise-here outside of the nix-build job?|]
    exitFailure
  let setResult result = do
        createDirectoryIfMissing True resultDir
        writeFileText (resultPath derivationName) (show result)
        removeFile (runningPath derivationName)
  unlessM (needsBuild derivationName) $ do
    setResult Success
    say [i|Build for #{derivationName} had already happened.|]
    exitSuccess
  flags <- filter (/= mempty) . splitOn " " . toText <$> getEnv "FLAGS"
  catch
    (nixStoreRealise derivationName flags)
    (\(err :: SomeException) -> do
      setResult Failure
      sayErr [i|nix-build failed with error #{err}.|]
      exitFailure
    )
  setResult Success
  say [i|Build for #{derivationName} successful.|]

nixStoreRealise :: Text -> [Text] -> IO ()
nixStoreRealise name flags = nix_store (["-r", name] <> flags)

ensureDeps :: Text -> IO ()
ensureDeps derivationName = do
  dependencies <- getDependenciesFromNix derivationName
  forConcurrently_ dependencies realise `catch` \(JobException e) ->
    throw [i|#{e}\nFailed dependency for #{derivationName}|]

-- Nothing means failing to acquire lock on the derivation name for starting the job.
tryQueue :: Text -> IO (Maybe Text)
tryQueue derivationName = handleExisting $ do
  createDirectoryIfMissing True runningDir
  bracket openNewFile closeFd getJobAndWrite
 where
  getJobAndWrite fd = do
    jobName <- startJob
    when (T.null jobName) $ throw [i|Laminarc returned an empty jobName.|]
    writeCount <- fdWrite fd (toString jobName)
    when (writeCount == 0)
      $ throw
          [i|Wrote 0 bytes of jobName "#{jobName}" to #{runningPath derivationName}|]
    pure . Just $ jobName
  startJob = do
    flags <- getEnv "FLAGS"
    decodeUtf8
      <$> (  laminarc "queue"
                      "nix-build"
                      ([i|DERIVATION=#{derivationName}|] :: Text)
                      ([i|FLAGS=#{flags}|] :: Text)
          |> captureTrim
          )
  handleExisting = handleJust
    (\x -> if isAlreadyExistsError x then Just x else Nothing)
    (const (pure Nothing))
  openNewFile = openFd (runningPath derivationName)
                       WriteOnly
                       (Just defaultMode)
                       defaultFileFlags { exclusive = True }
  defaultMode =
    ownerReadMode .|. ownerWriteMode .|. groupReadMode .|. otherReadMode

queueJobWithLaminarc :: Text -> IO Text
queueJobWithLaminarc derivationName = whenNothingM
  (do
    jobMay <- tryQueue derivationName
    whenJust jobMay $ \jobName ->
      say [i|Job #{jobName} started for #{derivationName}. Waiting ...|]
    pure jobMay
  )
  (ensureRunningJob derivationName)

ensureRunningJob :: Text -> IO Text
ensureRunningJob derivationName = whenNothingM
  (do
    jobMay <- getRunningJob derivationName
    whenJust jobMay $ \jobName ->
      say [i|Job #{jobName} running for #{derivationName}. Waiting ...|]
    pure jobMay
  )
  (queueJobWithLaminarc derivationName)

-- Nothing means there is no running Job.
getRunningJob :: Text -> IO (Maybe Text)
getRunningJob derivationName = poll 0
 where
  path    = runningPath derivationName
  request = handleNoExist (Just <$> readFileText path)
  handleNoExist =
    handleJust (guard . isDoesNotExistError) (const $ pure Nothing)
  poll count = do
    mayJob <- request
    if count < 50 && mayJob == Just ""
      then threadDelay 10000 >> poll (count + 1)
      else do

        pure mayJob

getJobVar :: Text -> IO (TVar BuildState)
getJobVar derivationName =
  atomically
    $   readTVar jobMap
    >>= maybe makeVar pure
    .   Map.lookup derivationName
 where
  makeVar = do
    newVar <- newTVar Pending
    modifyTVar' jobMap (Map.insert derivationName newVar)
    pure newVar

realise :: Text -> IO ()
realise derivationName = do
  jobVar  <- getJobVar derivationName
  now     <- getCurrentTime
  runHere <- atomically $ do
    jobState <- readTVar jobVar
    case jobState of
      Complete  -> pure False
      Running _ -> retry
      Pending   -> do
        writeTVar jobVar (Running now)
        pure True
  when runHere $ do
    say [i|Requiring #{derivationName}...|]
    ensureDeps derivationName
    needBuild <- needsBuild derivationName
    if needBuild
      then runBuild now
      else say [i|#{derivationName} was already built.|]
  atomically (writeTVar jobVar Complete)
 where
  runBuild start = do
    jobName <- ensureRunningJob derivationName
    handleWaitFail $ waitForJob derivationName >>= \case
      Success -> do
        now <- getCurrentTime
        say
          [i|Job #{jobName} completed for #{derivationName} after #{formatTime defaultTimeLocale "%2h:%2M:%2S" (diffUTCTime now start)}.|]
      Failure -> throw [i|Job #{jobName} failed #{derivationName}.|]
  processWaitFail (WaitException e) = do
    sayErr
      [i|Retrying to find or create a job for #{derivationName} after waiting for job failed with error "#{e}" |]
    realise derivationName
  handleWaitFail = handle processWaitFail

checkStaleness :: IO ()
checkStaleness = forever $ do
  handleJust (guard . isDoesNotExistError) (const pass) $ do
    nothingQueued <-
      T.null . decodeUtf8 <$> (laminarc "show-queued" |> captureTrim)
    when nothingQueued $ do
      knownJobs <-
        fmap strip
        .   lines
        .   decodeUtf8
        <$> (laminarc "show-running" |> captureTrim)
      jobs <- Map.toList <$> readTVarIO jobMap
      forConcurrently_ jobs $ \(derivationName, jobVar) ->
        checkStalenessFor knownJobs jobVar derivationName
  threadDelay 60000000

checkStalenessFor :: [Text] -> TVar BuildState -> Text -> IO ()
checkStalenessFor jobs jobVar derivationName =
  whenJustM (running <$> readTVarIO jobVar) $ \start ->
    whenJustM (getRunningJob derivationName) $ \jobName -> do
      now <- getCurrentTime
      say
        [i|Still waiting for job #{jobName} for #{derivationName} after #{formatTime defaultTimeLocale "%2h:%2M:%2S" (diffUTCTime now start)} ...|]
      fileTime <- getModificationTime (runningPath derivationName)
      let notRunning = not $ any (`isInfixOf` jobName) jobs
          oldEnough  = diffUTCTime now fileTime > 60
          stale      = notRunning && oldEnough
      when stale $ do
        removeFile (runningPath derivationName)
        sayErr
          [i|File #{runningPath derivationName} claiming job name "#{jobName}" seems to be stale. Deleting File.|]
 where
  running (Running a) = Just a
  running _           = Nothing

waitForJob :: Text -> IO JobResult
waitForJob derivationName = do
  done <- newTVarIO False
  let finished = atomically (writeTVar done True)
  withManager $ \manager -> do
    _ <- watchDir manager runningDir fileDeleted (const finished)
    withAsync (whenNothingM_ (getRunningJob derivationName) finished)
              (const $ atomically $ readTVar done >>= check)
  resultText <-
    handleJust
        (guard . isDoesNotExistError)
        (const $ throwWait
          [i|Job result file #{resultPath derivationName} does not exist.|]
        )
      $ readFile (resultPath derivationName)
  maybe
      (throwWait [i|Failed to parse result from #{resultPath derivationName}.|])
      pure
    . readMaybe
    . toString
    $ resultText
 where
  fileDeleted (Removed a _ _) | a == runningPath derivationName = True
  fileDeleted _ = False


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  whenNotNullM missingExecutables $ \x -> do
    sayErr [i|Missing executables #{show x}|]
    exitFailure
  args <- fmap toText <$> getArgs
  handle (\(JobException e) -> sayErr e >> exitFailure) $ case args of
    ["realise-here", derivationName] -> job derivationName
    ["realise"     , derivationName] -> do
      jobId <- getEnv "JOB"
      runId <- getEnv "RUN"
      setEnv "LAMINAR_REASON"
             [i|Building #{derivationName} in #{jobId}:#{runId}|]
      race_ (realise derivationName) checkStaleness
    _ ->
      sayErr "Usage: realise-here <derivationName> | realise <derivationName>"
