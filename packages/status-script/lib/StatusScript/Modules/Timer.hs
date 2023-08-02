module StatusScript.Modules.Timer (timers) where

import Data.Aeson qualified as Aeson
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.FileWatch qualified as FileWatch
import System.Environment qualified as Env
import System.FSNotify qualified as Notify

data Timer = MkTimer
  { name :: Text
  , at :: Int
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

timers :: R.MonadHeadlessApp t m => Notify.WatchManager -> m (R.Event t [Timer])
timers watch_manager = do
  home <- liftIO $ Env.getEnv "HOME"
  FileWatch.watchFileContents watch_manager home ".timers" <&> fmap encodeUtf8 % R.mapMaybe Aeson.decode'
