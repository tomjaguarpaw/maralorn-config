module StatusScript.Mode (getMode, Mode (..)) where

import Maralorn.Prelude
import Reflex (constDyn)
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.Env (Env (..))

data Mode = Klausur | Orga | Code | Leisure | Unrestricted | Default deriving (Eq, Ord, Show, Enum, Bounded)

getMode :: R.MonadHeadlessApp t m => Env -> m (R.Dynamic t Mode)
getMode _env = pure $ constDyn Default

-- FileWatch.watchFileContents env env.homeDir ".mode" <<&>> \case
--  Nothing -> Default
--  Just name ->
--    find (\mode -> name == Text.toLower (show mode)) modes
--      & fromMaybe (error [i|Unknown mode #{name}|])

-- modes :: [Mode]
-- modes = enumFrom Klausur
