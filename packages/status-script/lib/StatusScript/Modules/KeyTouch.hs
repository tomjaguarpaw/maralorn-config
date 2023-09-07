module StatusScript.Modules.KeyTouch (touchRequired) where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env)
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["yubikey-touch-detector"]
missingExecutables :: IO [FilePath]
touchRequired :: (R.MonadHeadlessApp t m) => Env -> m (R.Dynamic t Bool)
touchRequired = \env -> do
  CommandUtil.reportMissing missingExecutables
  touch_event <-
    ReflexUtil.processLines env (yubikey_touch_detector "-stdout")
  R.holdDyn
    False
    do
      touch_event & R.mapMaybe \case
        "U2F_1" -> Just True
        "U2F_0" -> Just False
        _ -> Nothing
