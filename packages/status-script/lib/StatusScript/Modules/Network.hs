module StatusScript.Modules.Network where

import Maralorn.Prelude hiding (catMaybes)
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import System.Which (which)
import Witherable (catMaybes)

networkState :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Text])
networkState = \env ->
  liftIO (which "nmcli") >>= \case
    Nothing -> do
      sayErr "No nmcli in PATH, disabling networkmanager support."
      pure R.never
    Just _ -> do
      monitor_event <- ReflexUtil.processLines env (Shh.exe "nmcli" "monitor")
      catMaybes <$> ReflexUtil.performEventThreaded env monitor_event \_ -> do
        retryTimeout 2 10 (Shh.exe "nmcli" "-g" "name" "connection" "show" "--active" |> Shh.captureTrim)
          <&> fmap (decodeUtf8 >>> lines >>> filter (/= "lo"))
