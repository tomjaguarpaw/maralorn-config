module StatusScript.Modules.BootState (bootState) where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))

Shh.load Shh.Absolute ["readlink"]
missingExecutables :: IO [FilePath]
bootState :: R.MonadHeadlessApp t m => Env -> R.Dynamic t Mode -> m (R.Event t [Warning])
bootState = \env mode -> do
  CommandUtil.reportMissing missingExecutables
  current_system <- FileWatch.watchFile env "/run/" "current-system"
  booted_system <- FileWatch.watchFile env "/run/" "booted-system"
  ReflexUtil.performEventThreaded env (ReflexUtil.taggedAndUpdated mode (R.leftmost [current_system, booted_system])) \case
    Klausur -> pure []
    _ -> do
      current_kernel <- CommandUtil.tryCmd (readlink "/run/current-system/kernel")
      booted_kernel <- CommandUtil.tryCmd (readlink "/run/booted-system/kernel")
      pure
        [ MkWarning
          { description = Just "Booted kernel stale"
          , group = "warning"
          , subgroup = Nothing
          }
        | current_kernel /= booted_kernel
        ]
