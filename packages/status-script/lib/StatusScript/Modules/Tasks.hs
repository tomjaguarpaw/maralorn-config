module StatusScript.Modules.Tasks (tasks) where

import Maralorn.Prelude
import Maralorn.Taskwarrior qualified
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.FilePath ((</>))
import Taskwarrior.Task as Task

tasks ::
  R.MonadHeadlessApp t m =>
  Env ->
  R.Dynamic t Mode ->
  m (R.Event t [Warning])
tasks env mode = do
  tasks_update <- FileWatch.watchFile env (env.homeDir </> ".task") "pending.data" <&> ReflexUtil.taggedAndUpdated mode
  ReflexUtil.performEventThreaded env tasks_update $
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
