module StatusScript.Mode (getMode, Mode (..)) where

import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.FileWatch qualified as FileWatch
import System.Environment qualified as Env
import System.FSNotify qualified as Notify

data Mode = Klausur | Orga | Code | Leisure | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)

getMode :: R.MonadHeadlessApp t m => Notify.WatchManager -> m (R.Dynamic t Mode)
getMode watch_manager = do
  home <- liftIO $ Env.getEnv "HOME"
  content_event <- FileWatch.watchFileContents watch_manager home ".mode"
  R.holdDyn Klausur $
    content_event <&> \name ->
      find (\mode -> name == Text.toLower (show mode)) modes
        & fromMaybe (error [i|Unknown mode #{name}|])

modes :: [Mode]
modes = enumFrom Klausur
