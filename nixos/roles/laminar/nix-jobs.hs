{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-missing-signatures -Wno-type-defaults -Wno-orphans #-}

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( forConcurrently )
import           Control.Exception              ( IOException
                                                , bracket
                                                , catch
                                                , handle
                                                )
import           Data.Bits                      ( Bits((.|.)) )
import qualified Data.Sequence                 as Seq
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import           Relude
import           Say                            ( say
                                                , sayErr
                                                )
import           Shh                            ( ExecArg
                                                , ExecReference(Absolute)
                                                , captureTrim
                                                , load
                                                , (|>)
                                                )
import           System.Directory               ( doesFileExist
                                                , removeFile
                                                )
import           System.Environment             ( getArgs
                                                , getEnv
                                                )
import           System.FSNotify                ( Event(Removed)
                                                , stopManager
                                                , watchDir
                                                , withManager
                                                )
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

instance Semigroup JobResult where
  Success <> Success = Success
  _       <> _       = Failure

instance Monoid JobResult where
  mempty = Success
-- 2 Entry points
-- 1. Realise i.e. "wait-for-and-trigger-build-if-necessary"
-- 2. Job i.e. "the actual realisation"

instance ExecArg Text where

getDependenciesFromNix :: Text -> IO (Seq Text)
getDependenciesFromNix derivationName = do
  everythingToDo <- nixStoreRealiseDryRun derivationName
  pure (Seq.filter (/= derivationName) everythingToDo)

nixStoreRealiseDryRun :: Text -> IO (Seq Text)
nixStoreRealiseDryRun derivationName =
  process <$> (nix_store "-r" derivationName "--dry-run" |> captureTrim)
 where
  process =
    fromList
      . dropWhile (/= "these derivations will be built:")
      . fmap T.strip
      . lines
      . decodeUtf8

job :: Text -> IO ()
job derivationName = do
  say [i|Initiating realisation for #{derivationName}.|]
  pathInfo <- getPathInfo derivationName
  let setResult result = do
        writeFileText (resultPath pathInfo) (show result)
        removeFile (runningPath pathInfo)
  ensureDeps True derivationName >>= \case
    Success -> do
      say [i|All dependencies provided, starting build for #{derivationName}.|]
      catch
        (nixStoreRealise derivationName)
        (\(err :: SomeException) -> do
          setResult Failure
          sayErr [i|nix-build failed with error #{err}.|]
          exitFailure
        )
      setResult Success
      say [i|Build for #{derivationName} successful. Finishing.|]
    Failure -> do
      sayErr
        [i|Couldn‘t build #{derivationName} because of failing dependency.|]

nixStoreRealise :: Text -> IO ()
nixStoreRealise = nix_store "-r"

ensureDeps :: Bool -> Text -> IO JobResult
ensureDeps topLevel derivationName = do
  dependencies <- getDependenciesFromNix derivationName
  when topLevel $ forM_ dependencies $ \dep ->
    say [i|Requiring build of #{dep}.|]
  fold <$> forConcurrently dependencies (realise topLevel)

-- Nothing means failing to acquire lock on the derivation name for starting the job.
tryQueue :: PathInfo -> IO (Maybe Text)
tryQueue pathInfo@(_, _, derivationName) = getRunningJob pathInfo >>= \case
  Nothing -> handle handleIOException $ bracket openNewFile closeFd $ \fd -> do
    jobName <-
      decodeUtf8
        <$> (  laminarc "queue" "nix-build" ([i|DERIVATION=#{derivationName}|] :: Text)
            |> captureTrim
            )
    writeCount <- fdWrite fd (toString jobName)
    when (writeCount == 0) $ sayErr
      [i|Wrote 0 bytes of jobName "#{jobName}" to #{runningPath pathInfo}|]
    pure (Just jobName)
  Just _ -> pure Nothing
 where
  openNewFile = openFd (runningPath pathInfo)
                       WriteOnly
                       (Just defaultMode)
                       defaultFileFlags { exclusive = True }
  defaultMode =
    ownerReadMode .|. ownerWriteMode .|. groupReadMode .|. otherReadMode
  handleIOException :: IOException -> IO (Maybe Text)
  handleIOException _ = pure Nothing

-- Nothing means a dependency failed.
queueJobWithLaminarc :: Bool -> PathInfo -> IO (Maybe Text)
queueJobWithLaminarc topLevel pathInfo@(_, _, derivationName) =
  tryQueue pathInfo >>= \case
    Nothing -> ensureRunningJob topLevel pathInfo
    a       -> do
      say [i|Queued build job ${jobName} for #{derivationName}.|]
      pure a

ensureRunningJob :: Bool -> PathInfo -> IO (Maybe Text)
ensureRunningJob topLevel pathInfo@(_, _, derivationName) =
  getRunningJob pathInfo >>= \case
    Nothing -> do
      ensureDeps False derivationName >>= \case
        Success -> queueJobWithLaminarc topLevel pathInfo
        Failure -> do
          sayErr
            [i|Could not realise #{derivationName} because of failed dependency.|]
          pure Nothing
    Just jobName -> do
      when topLevel
        $ say [i|Waiting for job #{jobName} running for #{derivationName}|]
      pure (Just jobName)


type PathInfo = (Text, Text, Text)

getPathInfo :: Text -> IO PathInfo
getPathInfo derivationName = do
  workSpace <- toText <$> getEnv "WORKSPACE"
  (workSpace, , derivationName)
    .   decodeUtf8
    <$> (nix_store "-q" "--hash" derivationName |> captureTrim)

runningDir :: PathInfo -> String
runningDir (workSpace, _, _) = [i|#{workSpace}/running-jobs|]
runningPath :: PathInfo -> String
runningPath (workSpace, drvHash, _) = [i|#{workSpace}/running-jobs/#{drvHash}|]
resultPath :: PathInfo -> String
resultPath (workSpace, drvHash, _) =
  [i|#{workSpace}/completed-jobs/#{drvHash}|]

-- Nothing means there is no running Job.
getRunningJob :: PathInfo -> IO (Maybe Text)
getRunningJob p = do
  let path = runningPath p
  pathExists <- doesFileExist path
  if pathExists
    then catch (Just <$> readFileText path)
               (\(_ :: IOException) -> getRunningJob p)
    else pure Nothing

realise :: Bool -> Text -> IO JobResult
realise topLevel derivationName = do
  pathInfo   <- getPathInfo derivationName
  jobNameMay <- ensureRunningJob topLevel pathInfo
  case jobNameMay of
    Just jobName -> do
      result <- waitForJob pathInfo
      case result of
        Success -> when topLevel
          $ say [i|#{derivationName} succesfully realised by job #{jobName}|]
        Failure -> sayErr [i|#{derivationName} failed in job #{jobName}|]
      pure result
    Nothing -> pure Failure

waitForJob :: PathInfo -> IO JobResult
waitForJob pathInfo = do
  _ <- withManager $ \manager -> do
    _ <- watchDir manager
                  (runningDir pathInfo)
                  fileDeleted
                  (const $ stopManager manager)
    maybeJob <- getRunningJob pathInfo
    whenNothing_ maybeJob $ stopManager manager
    forever $ threadDelay 1000000
  readFileText (resultPath pathInfo)
    >>= (\case
          Nothing -> do
            sayErr [i|Failed to parse result from #{resultPath pathInfo}|]
            pure Failure
          Just a -> pure a
        )
    .   readMaybe
    .   toString
 where
  fileDeleted (Removed a _ _) | a == runningPath pathInfo = True
  fileDeleted _ = False


main :: IO ()
main = do
  whenNotNullM missingExecutables $ \x -> do
    sayErr [i|Missing executables #{show x}|]
    exitFailure
  args <- fmap toText <$> getArgs
  case args of
    ["realise-here", derivationName] -> job derivationName
    ["realise"     , derivationName] -> realise True derivationName >>= \case
      Success -> exitSuccess
      Failure -> exitFailure
    _ ->
      sayErr "Usage: realise-here <derivationName> | realise <derivationName>"
