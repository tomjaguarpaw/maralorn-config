module StatusScript.Modules.Audio where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["pactl"]
missingExecutables :: IO [FilePath]
audioUpdateEvent :: R.MonadHeadlessApp t m => m (R.Event t ())
audioUpdateEvent = do
  ReflexUtil.reportMissing missingExecutables
  ReflexUtil.processLines (pactl "subscribe") <<&>> const ()
