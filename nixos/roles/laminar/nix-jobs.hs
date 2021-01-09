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
import           Control.Concurrent.STM         ( check )
import           Control.Exception              ( bracket
                                                , catch
                                                , handle
                                                , handleJust
                                                , throwIO
                                                )
import           Data.Bits                      ( Bits((.|.)) )
import qualified Data.Sequence                 as Seq
import           Data.String.Interpolate        ( i )
import           Data.Text                      ( isInfixOf
                                                , splitOn
                                                , strip
                                                )
import qualified Data.Text                     as T
import           Data.Time                      ( diffUTCTime
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
                                                )
import           System.FSNotify                ( Event(Removed)
                                                , watchDir
                                                , withManager
                                                )
import           System.IO                      ( BufferMode(LineBuffering)
                                                , hSetBuffering
                                                )
import           System.IO.Error
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

data ReportLevel = None | Self | Children deriving (Show, Eq, Ord, Enum)

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

whenSelf level = when (level >= Self)
whenChildren level = when (level >= Children)
levelPrec Children = Self
levelPrec _        = None

drvBasename derivationName =
  fromMaybe derivationName . viaNonEmpty last $ splitOn "/" derivationName

workspace, resultDir, runningDir :: String
workspace = "/var/lib/laminar/run/nix-build/workspace"
resultDir = [i|#{workspace}/completed-jobs|]
runningDir = [i|#{workspace}/running-jobs|]
runningPath, resultPath :: Text -> String
runningPath p = [i|#{runningDir}/#{drvBasename p}|]
resultPath p = [i|#{resultDir}/#{drvBasename p}|]

-- Bool means derivation itself needs to be build
getDependenciesFromNix :: Text -> IO (Seq Text, Bool)
getDependenciesFromNix derivationName = do
  everythingToDo <- nixStoreRealiseDryRun derivationName
  let depsToDo = Seq.filter (/= derivationName) everythingToDo
  pure (depsToDo, everythingToDo /= depsToDo)

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
  needBuild <- ensureDeps Children derivationName
  unless needBuild $ do
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

ensureDeps :: ReportLevel -> Text -> IO Bool
ensureDeps level derivationName = do
  (dependencies, needBuild) <- getDependenciesFromNix derivationName
  whenChildren level $ forM_ dependencies $ \dep -> say [i|Requiring #{dep}.|]
  forConcurrently_ dependencies (realise $ levelPrec level)
    `catch` \(JobException e) ->
              throw [i|#{e}\nFailed dependency for #{derivationName}|]
  pure needBuild

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

queueJobWithLaminarc :: ReportLevel -> Text -> IO Text
queueJobWithLaminarc level derivationName =
  whenNothingM (tryQueue derivationName) (ensureRunningJob level derivationName)

ensureRunningJob :: ReportLevel -> Text -> IO Text
ensureRunningJob level derivationName = whenNothingM
  (getRunningJob derivationName)
  (queueJobWithLaminarc level derivationName)

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
      else pure mayJob

realise :: ReportLevel -> Text -> IO ()
realise level derivationName = do
  needBuild <- ensureDeps level derivationName
  if needBuild
    then runBuild
    else whenSelf level $ say [i|#{derivationName} was already built.|]
 where
  runBuild = do
    jobName <- ensureRunningJob level derivationName
    whenSelf level
      $ say [i|Job #{jobName} running for #{derivationName}. Waiting ...|]
    handleWaitFail $ waitForJob derivationName >>= \case
      Success ->
        whenSelf level $ say [i|Job #{jobName} completed #{derivationName}.|]
      Failure -> throw [i|Job #{jobName} failed #{derivationName}.|]
  processWaitFail (WaitException e) = do
    whenSelf level
      $ sayErr
          [i|Retrying to find or create a job for #{derivationName} after waiting for job failed with error "#{e}" |]
    realise level derivationName
  handleWaitFail = handle processWaitFail

checkStaleness :: Text -> IO ()
checkStaleness derivationName = forever $ do
  whenJustM (getRunningJob derivationName) $ \jobName ->
    handleJust (guard . isDoesNotExistError) (const pass) $ do
      nothingQueued <-
        T.null . decodeUtf8 <$> (laminarc "show-queued" |> captureTrim)
      knownJobs <-
        fmap strip
        .   lines
        .   decodeUtf8
        <$> (laminarc "show-running" |> captureTrim)
      now      <- getCurrentTime
      fileTime <- getModificationTime (runningPath derivationName)
      let notRunning = not $ any (`isInfixOf` jobName) knownJobs
          oldEnough  = diffUTCTime now fileTime > 60
          stale      = notRunning && nothingQueued && oldEnough
      when stale $ do
        removeFile (runningPath derivationName)
        throwWait
          [i|File #{runningPath derivationName} claiming job name "#{jobName}" seems to be stale. Deleting File.|]
  threadDelay 10000000

waitForJob :: Text -> IO JobResult
waitForJob derivationName = do
  done <- newTVarIO False
  let finished = atomically (writeTVar done True)
  withManager $ \manager -> do
    _ <- watchDir manager runningDir fileDeleted (const finished)
    withAsync
      (whenNothingM_ (getRunningJob derivationName) finished)
      (const $ race_ (atomically $ readTVar done >>= check)
                     (checkStaleness derivationName)
      )
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
  case args of
    ["realise-here", derivationName] -> job derivationName
    ["realise"     , derivationName] -> realise Children derivationName
    _ ->
      sayErr "Usage: realise-here <derivationName> | realise <derivationName>"
