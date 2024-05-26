module StatusScript.Modules.IdleState where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["journalctl"]

missingExecutables :: IO [FilePath]

data IdleState = Off | Active | Idle Int
  deriving stock (Generic, Eq)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

idleState :: R.MonadHeadlessApp t m => Env -> m (R.Dynamic t IdleState)
idleState = \env -> do
  service_running_dyn <-
    ReflexUtil.processLines env (journalctl "--user" "-n1" "-fu" "swayidle.service")
      <<&>> \case
        line | "Stopped" `BS.isInfixOf` line -> Just False
        line | "Started" `BS.isInfixOf` line -> Just True
        _ -> Nothing
      <&> R.mapMaybe id
      >>= R.holdDyn True
  file_dyn <-
    FileWatch.watchFileContents env env.homeDir ".idle_state"
      <&> R.updated
        % R.mapMaybe id
        % fmap encodeUtf8
        % R.mapMaybe Aeson.decode'
      >>= R.holdDyn Active
  R.holdUniqDyn (liftA2 combine service_running_dyn file_dyn)

combine :: Bool -> IdleState -> IdleState
combine True x = x
combine False _ = Off
