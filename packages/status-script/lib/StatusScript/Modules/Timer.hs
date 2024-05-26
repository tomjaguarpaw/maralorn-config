module StatusScript.Modules.Timer (timers) where

import Data.Aeson qualified as Aeson
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch

data Timer = MkTimer
  { name :: Text
  , at :: Int
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

timers :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Timer])
timers = \env ->
  FileWatch.watchFileContents env env.homeDir ".timers"
    <&> R.updated
    % R.fmapMaybe id
    % fmap encodeUtf8
    % R.mapMaybe Aeson.decode'
