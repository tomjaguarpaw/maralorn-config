module StatusScript.Modules.Network where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import System.Which (which)

networkState :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Text])
networkState = \env ->
  liftIO (which "nmcli") >>= \case
    Nothing -> do
      sayErr "No nmcli in PATH, disabling networkmanager support."
      pure R.never
    Just _ -> do
      monitor_event <- ReflexUtil.processLines env (Shh.exe "nmcli" "monitor")
      ReflexUtil.performEventThreaded env monitor_event \_ -> do
        retryWithBackoff (Shh.exe "nmcli" "-g" "name" "connection" "show" "--active" |> Shh.captureTrim)
          <&> decodeUtf8
          % lines
          % filter (/= "lo")
