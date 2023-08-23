module StatusScript.Modules.ConfigStale (configStale) where

import Control.Exception qualified as Exception
import Data.ByteString.Char8 qualified as ByteStringChar
import Data.Set qualified as Set
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.FilePath ((</>))

configStale :: (R.MonadHeadlessApp t m) => Env -> R.Dynamic t Mode -> R.Dynamic t (Set Text) -> m (R.Event t [Warning])
configStale env mode dirties = do
  let home = env.homeDir
      git_dir = home </> "git"
      modes_dir = home </> ".volatile" </> "modes"
      config_dirty = dirties <&> Set.member "config"
  commit_event <- FileWatch.watchFile env (git_dir </> "config/.git/refs/heads") "main"
  system_commit_event <- FileWatch.watchFile env "/run/current-system" "config-commit"
  modes_commit_event <- FileWatch.watchFile env modes_dir "config-commit"
  tick <- ReflexUtil.tickEvent 30
  ReflexUtil.performEventThreaded env (ReflexUtil.taggedAndUpdated ((,) <$> mode <*> config_dirty) (R.leftmost [commit_event, system_commit_event, modes_commit_event, tick])) \case
    (Klausur, _) -> pure []
    (_, True) -> pure []
    _ -> do
      commit <- readFileBS (git_dir </> "config/.git/refs/heads/main") & Exception.try @Exception.IOException %> hush
      system_commit <- readFileBS "/run/current-system/config-commit" & Exception.try @Exception.IOException %> hush
      modes_commit <- readFileBS (modes_dir </> "config-commit") & Exception.try @Exception.IOException %> hush
      let is_dirty = \installed_commit -> case (commit, installed_commit) of
            (Nothing, _) -> False
            (Just a, Just b) | ByteStringChar.strip a == ByteStringChar.strip b -> False
            _ -> True
      let stale_warn = \scope ->
            MkWarning
              { description = Just [i|Current #{scope} stale|]
              , group = "warning"
              , subgroup = Nothing
              }
      pure $ [stale_warn "system" | is_dirty system_commit] <> [stale_warn "home" | is_dirty modes_commit]
