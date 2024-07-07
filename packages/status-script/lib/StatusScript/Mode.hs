module StatusScript.Mode (getMode, Mode (..), modeIcon) where

import Data.Aeson (ToJSON)
import Data.String.Interpolate (i)
import Data.Text (strip, toLower)
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Relude
import StatusScript.Env (Env (..))
import StatusScript.FileWatch

data Mode = DND | Normal | Sort
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON)

getMode :: R.MonadHeadlessApp t m => Env -> m (R.Dynamic t Mode)
getMode env =
  watchFileContents env env.homeDir ".mode" <&> fmap \case
    Nothing -> Normal
    Just name ->
      find (\mode -> strip (toLower name) == toLower (show mode)) modes
        & fromMaybe (error [i|Unknown mode #{name}|])

modeIcon :: Mode -> Maybe Char
modeIcon = \case
  DND -> Just $ toEnum 0xf15e2 -- nf-md-comment_off_outline
  Normal -> Nothing
  Sort -> Just $ toEnum 0xeb86 -- nf-cod-list_tree

modes :: [Mode]
modes = enumFrom DND
