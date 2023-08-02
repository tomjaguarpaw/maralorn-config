module StatusScript.Modules.Git (gitEvents) where

import Control.Exception qualified as Exception
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Reflex.Network qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.FSNotify qualified as Notify
import System.FilePath ((</>))

Shh.load Shh.Absolute ["git"]
missingExecutables :: IO [FilePath]
isDirty :: String -> IO Bool
isDirty gitDir = ((/= "") <$> (git "--no-optional-locks" "-C" gitDir "status" "--porcelain" |> Shh.captureTrim)) `Exception.catch` (\(_ :: SomeException) -> pure True)

isUnpushed :: String -> IO Bool
isUnpushed gitDir = do
  revs <- CommandUtil.tryCmd (git "--no-optional-locks" "-C" gitDir "branch" "-r" "--contains" "HEAD")
  pure $ LBS.null revs

gitEvents :: R.MonadHeadlessApp t m => Notify.WatchManager -> R.Dynamic t Mode -> m (R.Event t [Warning], R.Dynamic t (Set Text))
gitEvents watch_manager mode = do
  start <- R.getPostBuild
  home <- liftIO $ Env.getEnv "HOME"
  let git_dir = home </> "git"
  CommandUtil.reportMissing missingExecutables
  git_dir_change <- (start <>) . void <$> FileWatch.watchDir watch_manager git_dir False (const True)
  git_dirs_event <- ReflexUtil.performEventThreaded git_dir_change \_ -> Directory.listDirectory git_dir
  let git_dirs_event' =
        git_dirs_event <&> \dirs -> do
          dir_update_events <- forM dirs \dir -> do
            dir_events <- forM [git_dir </> dir] \sub_dir -> FileWatch.watchDir watch_manager sub_dir False (const True)
            git_dir_event <- FileWatch.watchDir watch_manager (git_dir </> dir </> ".git") False (const True)
            git_refs_event <- FileWatch.watchDir watch_manager (git_dir </> dir </> ".git/refs") True (const True)
            pure $
              mconcat (void <$> dir_events)
                <> void git_dir_event
                <> void git_refs_event
                $> [dir]
          pure $ mconcat dir_update_events
  git_dir_events <- (<> git_dirs_event) . R.switchDyn <$> R.networkHold (pure R.never) git_dirs_event'
  let dirs_matching git_pred = do
        dirty_updates <- ReflexUtil.performEventThreaded git_dir_events \dirs -> do
          now_dirty <- Set.fromList . fmap toText <$> filterM (git_pred . (git_dir </>)) dirs
          pure (Set.difference (Set.fromList (fmap toText dirs)) now_dirty, now_dirty)
        R.foldDyn (\(now_clean, now_dirty) dirty -> Set.union now_dirty (Set.difference dirty now_clean)) mempty dirty_updates
  dirty_event <- dirs_matching isDirty
  unpushed_event <- dirs_matching isUnpushed
  warnings_event <-
    [(dirty_event, "dirty"), (unpushed_event, "unpushed")] & mapM \(event, subgroup) ->
      pure $ R.updated $ R.ffor2 mode event \mode' dirties ->
        let dirty_dirs = (if (mode' == Klausur) then Set.filter (== "promotion") else id) dirties
         in toList dirty_dirs <&> \dir ->
              MkWarning
                { description = dir
                , group = "git"
                , subgroup = Just subgroup
                }
  warning_event <- ReflexUtil.concatEvents warnings_event
  pure (warning_event, dirty_event)
