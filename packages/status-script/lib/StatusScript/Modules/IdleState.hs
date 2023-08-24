module StatusScript.Modules.IdleState where

import Data.Aeson qualified as Aeson
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch

data IdleState = MkIdleState
  { at :: Int
  , idle :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

idleState :: (R.MonadHeadlessApp t m) => Env -> m (R.Event t IdleState)
idleState = \env ->
  FileWatch.watchFileContents env env.homeDir ".idle_state"
    <&> R.updated
      % R.fmapMaybe id
      % fmap encodeUtf8
      % R.mapMaybe Aeson.decode'
