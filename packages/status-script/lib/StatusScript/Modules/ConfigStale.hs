module StatusScript.Modules.ConfigStale (configStale) where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import Data.ByteString.Char8 qualified as ByteStringChar
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.FilePath ((</>))

Shh.load Shh.Absolute ["readlink", "nix", "nix-diff", "jq"]
missingExecutables :: IO [FilePath]
diffIsSmall :: LBS.ByteString -> LBS.ByteString -> IO Bool
diffIsSmall = \pathA pathB -> (== "[]") <$> (nix_diff "--json" [pathA, pathB] |> jq ".inputsDiff.inputDerivationDiffs" |> Shh.captureTrim)

configStale :: R.MonadHeadlessApp t m => Env -> R.Dynamic t Mode -> R.Dynamic t (Set Text) -> m (R.Event t [Warning])
configStale env mode dirties = do
  CommandUtil.reportMissing missingExecutables
  commit_var <- newTVarIO ""
  system_var <- newTVarIO ""
  modes_var <- newTVarIO ""
  system_dirty_var <- newTVarIO False
  modes_dirty_var <- newTVarIO False
  host_name <- ByteStringChar.strip <$> readFileBS "/etc/hostname"
  let home = env.homeDir
      git_dir = home </> "git"
      modes_dir = home </> ".volatile" </> "modes"
      scan = do
        current_commit <- readFileBS (git_dir </> "config/.git/refs/heads/main")
        system_commit <- Exception.try @Exception.IOException do readFileBS "/run/current-system/config-commit"
        modes_commit <- Exception.try do readFileBS (modes_dir </> "config-commit")
        current_system <- readlink "/run/current-system" |> Shh.captureTrim
        current_modes <- readlink modes_dir |> Shh.captureTrim
        let stale_config = \case
              (Right commit) | commit == current_commit -> False
              _ -> True
            system_stale = stale_config system_commit
            modes_stale = stale_config modes_commit
        (commit_change, system_change, modes_change) <-
          atomically $
            (,,)
              <$> (STM.stateTVar commit_var \previous_commit -> (previous_commit /= current_commit, current_commit))
              <*> (STM.stateTVar system_var \previous_system -> (previous_system /= current_system, current_system))
              <*> (STM.stateTVar modes_var \previous_modes -> (previous_modes /= current_modes, current_modes))
        if system_stale
          then when (commit_change || system_change) do
            sayErr "Eval system config …"
            next_system <- nix "eval" "--raw" [s|#{home}/git/config\#nixosConfigurations.#{host_name}.config.system.build.toplevel.drvPath|] |> Shh.captureTrim
            sayErr "System eval finished."
            diff_is_small <- diffIsSmall next_system current_system
            atomically $ writeTVar system_dirty_var (not diff_is_small)
          else atomically do writeTVar system_dirty_var False
        if modes_stale
          then when (commit_change || modes_change) do
            sayErr "Eval home config …"
            next_modes <- nix "eval" "--raw" [s|#{home}/git/config\#homeModes.#{host_name}.drvPath|] |> Shh.captureTrim
            sayErr "Home eval finished."
            diff_is_small <- diffIsSmall next_modes current_modes
            atomically $ writeTVar modes_dirty_var (not diff_is_small)
          else atomically do writeTVar modes_dirty_var False
        system_dirty <- readTVarIO system_dirty_var
        modes_dirty <- readTVarIO modes_dirty_var
        let stale_warn = \scope ->
              MkWarning
                { description = Just [i|Current #{scope} stale|]
                , group = "warning"
                , subgroup = Nothing
                }
        pure $ [stale_warn "system" | system_dirty] <> [stale_warn "home" | modes_dirty]
  tick <- ReflexUtil.tickEvent 30
  let config_dirty = dirties <&> Set.member "config"
  ReflexUtil.performEventThreaded env (ReflexUtil.taggedAndUpdated ((,) <$> mode <*> config_dirty) tick) \case
    (Klausur, _) -> pure []
    (_, True) -> pure []
    _ -> scan
