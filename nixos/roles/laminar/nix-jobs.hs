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
import           System.Environment             ( getArgs )
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

getDependenciesFromNix :: Text -> IO (Seq Text)
getDependenciesFromNix derivationName = do
  everythingToDo <- nixStoreRealiseDryRun derivationName
  pure (Seq.filter (/= derivationName) everythingToDo)

nixStoreRealiseDryRun :: Text -> IO (Seq Text)
nixStoreRealiseDryRun derivationName = do
  process
    <$> (nix_store "-r" derivationName "--dry-run" &!> StdOut |> captureTrim)
 where
  process =
    fromList
      . drop 1
      . dropWhile (/= "these derivations will be built:")
      . fmap strip
      . lines
      . decodeUtf8

job :: Text -> IO ()
job derivationName = do
  say [i|Building #{derivationName}.|]
  let setResult result = do
        createDirectoryIfMissing True resultDir
        writeFileText (resultPath derivationName) (show result)
        removeFile (runningPath derivationName)
  ensureDeps Children derivationName
  catch
    (nixStoreRealise derivationName)
    (\(err :: SomeException) -> do
      setResult Failure
      sayErr [i|nix-build failed with error #{err}.|]
      exitFailure
    )
  setResult Success
  say [i|Build for #{derivationName} successful.|]

nixStoreRealise :: Text -> IO ()
nixStoreRealise = nix_store "-r"

ensureDeps :: ReportLevel -> Text -> IO ()
ensureDeps level derivationName = do
  dependencies <- getDependenciesFromNix derivationName
  whenChildren level $ forM_ dependencies $ \dep ->
    say [i|Requiring #{dep}.|]
  forConcurrently_ dependencies (realise $ levelPrec level)
    `catch` \(JobException e) ->
              throw [i|#{e}\nFailed dependency for #{derivationName}|]

-- Nothing means failing to acquire lock on the derivation name for starting the job.
tryQueue :: Text -> IO (Maybe Text)
tryQueue derivationName = getRunningJob derivationName >>= \case
  Nothing -> do
    createDirectoryIfMissing True runningDir
    handleJust (\x -> if isAlreadyExistsError x then Just x else Nothing)
               (const (pure Nothing))
      $ bracket openNewFile closeFd
      $ \fd -> do
          jobName <-
            decodeUtf8
              <$> (  laminarc "queue"
                              "nix-build"
                              ([i|DERIVATION=#{derivationName}|] :: Text)
                  |> captureTrim
                  )
          when (T.null jobName) $ throw [i|Laminarc returned an empty jobName.|]
          writeCount <- fdWrite fd (toString jobName)
          when (writeCount == 0)
            $ throw
                [i|Wrote 0 bytes of jobName "#{jobName}" to #{runningPath derivationName}|]
          pure . Just $ jobName
  Just _ -> pure Nothing
 where
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
getRunningJob derivationName = poll
 where
  path    = runningPath derivationName
  request = do
    pathExists <- doesFileExist path
    if pathExists
      then
        handleJust (guard . isDoesNotExistError) (const $ pure Nothing)
        $   Just
        <$> readFileText path
      else pure Nothing
  poll = go 0
   where
    go count = do
      mayJob <- request
      if count < 50 && mayJob == Just ""
        then threadDelay 10000 >> go (count + 1)
        else pure mayJob

realise :: ReportLevel -> Text -> IO ()
realise level derivationName = do
  ensureDeps level derivationName
  jobName <- ensureRunningJob level derivationName
  whenSelf level
    $ say [i|Job #{jobName} running for #{derivationName}. Waiting ...|]
  handle
      (\(WaitException e) -> do
        whenSelf level
          $ sayErr
              [i|Retrying to find or create a job for #{derivationName} after waiting for job failed with error "#{e}" |]
        realise level derivationName
      )
    $ do
        waitForJob derivationName >>= \case
          Success -> whenSelf level
            $ say [i|Job #{jobName} completed #{derivationName}.|]
          Failure -> throw [i|Job #{jobName} failed #{derivationName}.|]

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
