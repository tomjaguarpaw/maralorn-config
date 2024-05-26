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
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.Directory qualified as Directory
import System.FilePath ((</>))

Shh.load Shh.Absolute ["git"]

missingExecutables :: IO [FilePath]
isDirty :: String -> IO Bool
isDirty gitDir =
  ((/= "") <$> (git "--no-optional-locks" "-C" gitDir "status" "--porcelain" |> Shh.captureTrim))
    `Exception.catch` (\(_ :: SomeException) -> pure True)

isUnpushed :: String -> IO Bool
isUnpushed gitDir = do
  revs <- CommandUtil.tryCmd (git "--no-optional-locks" "-C" gitDir "branch" "-r" "--contains" "HEAD")
  pure $ LBS.null revs

gitEvents :: R.MonadHeadlessApp t m => Env -> R.Dynamic t Mode -> m (R.Event t [Warning], R.Dynamic t (Set Text))
gitEvents env mode = do
  start <- R.getPostBuild
  let git_dir = env.homeDir </> "git"
  CommandUtil.reportMissing missingExecutables
  dir_event <- FileWatch.watchDir env git_dir False (const True) >>= R.throttle 0.2
  let git_dir_change = start <> void dir_event
  git_dirs_event <- ReflexUtil.performEventThreaded env git_dir_change \_ -> Directory.listDirectory git_dir
  let git_dirs_event' =
        git_dirs_event <&> \dirs -> do
          dir_update_events <- forM dirs \dir -> do
            dir_events <- forM [git_dir </> dir] \sub_dir -> FileWatch.watchDir env sub_dir False (const True)
            git_dir_event <- FileWatch.watchDir env (git_dir </> dir </> ".git") False (const True)
            git_refs_event <- FileWatch.watchDir env (git_dir </> dir </> ".git/refs") True (const True)
            pure $
              mconcat (void <$> dir_events)
                <> void git_dir_event
                <> void git_refs_event
                $> [dir]
          R.throttle 0.2 $ mconcat dir_update_events
  git_dir_events <- (<> git_dirs_event) . R.switchDyn <$> R.networkHold (pure R.never) git_dirs_event'
  let dirs_matching git_pred = do
        dirty_updates <- ReflexUtil.performEventThreaded env git_dir_events \dirs -> do
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
                { description = Just dir
                , group = "git"
                , subgroup = Just subgroup
                }
  warning_event <- ReflexUtil.concatEvents warnings_event
  pure (warning_event, dirty_event)
