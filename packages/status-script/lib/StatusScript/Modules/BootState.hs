module StatusScript.Modules.BootState (bootState) where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.FSNotify qualified as Notify

Shh.load Shh.Absolute ["readlink"]
missingExecutables :: IO [FilePath]
bootState :: R.MonadHeadlessApp t m => Notify.WatchManager -> R.Dynamic t Mode -> m (R.Event t [Warning])
bootState watch_manager mode = do
  CommandUtil.reportMissing missingExecutables
  current_system <- FileWatch.watchFile watch_manager "/run/" "current-system"
  booted_system <- FileWatch.watchFile watch_manager "/run/" "booted-system"
  ReflexUtil.performEventThreaded (ReflexUtil.taggedAndUpdated mode (R.leftmost [current_system, booted_system])) \case
    Klausur -> pure []
    _ -> do
      current_kernel <- readlink "/run/current-system/kernel" |> Shh.captureTrim
      booted_kernel <- readlink "/run/booted-system/kernel" |> Shh.captureTrim
      pure [MkWarning{description = "Booted kernel stale", group = "warning", subgroup = Nothing} | current_kernel /= booted_kernel]
