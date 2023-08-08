module StatusScript.Mode (getMode, Mode (..)) where

import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch

data Mode = Klausur | Orga | Code | Leisure | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)

getMode :: R.MonadHeadlessApp t m => Env -> m (R.Dynamic t Mode)
getMode env = do
  content_event <- FileWatch.watchFileContents env env.homeDir ".mode"
  R.holdDyn
    Klausur
    ( content_event <&> \name ->
        find (\mode -> name == Text.toLower (show mode)) modes
          & fromMaybe (error [i|Unknown mode #{name}|])
    )

modes :: [Mode]
modes = enumFrom Klausur
