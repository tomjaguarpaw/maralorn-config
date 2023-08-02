module StatusScript.Modules.Audio (audioUpdateEvent) where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["pactl"]
missingExecutables :: IO [FilePath]
audioUpdateEvent :: R.MonadHeadlessApp t m => m (R.Event t ())
audioUpdateEvent = do
  CommandUtil.reportMissing missingExecutables
  ReflexUtil.processLines (pactl "subscribe") <<&>> const ()
