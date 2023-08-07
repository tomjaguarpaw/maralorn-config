module StatusScript.Modules.Tasks (tasks) where

import Maralorn.Prelude
import Maralorn.Taskwarrior qualified
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.Environment qualified as Env
import System.FSNotify qualified as Notify
import System.FilePath ((</>))
import Taskwarrior.Task as Task

tasks ::
  R.MonadHeadlessApp t m =>
  Notify.WatchManager ->
  R.Dynamic t Mode ->
  m (R.Event t [Warning])
tasks watch_manager mode = do
  home <- liftIO $ Env.getEnv "HOME"
  tasks_update <- FileWatch.watchFile watch_manager (home </> ".task") "pending.data" <&> ReflexUtil.taggedAndUpdated mode
  ReflexUtil.performEventThreaded tasks_update $
    \case
      Orga ->
        ( Maralorn.Taskwarrior.getInbox <<&>> \task ->
            MkWarning
              { description = Just (task.description <> maybe "" (\num -> [i| (#{num})|]) task.id)
              , group = "inbox"
              , subgroup = Nothing
              }
        )
      _ -> pure []
