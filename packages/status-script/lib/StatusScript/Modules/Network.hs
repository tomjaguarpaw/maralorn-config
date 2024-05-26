module StatusScript.Modules.Network where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil

networkState :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Text])
networkState = \env ->
  liftIO Shh.pathBins <&> (elem "nmcli") >>= \case
    False -> do
      sayErr "No nmcli in PATH, disabling networkmanager support."
      pure R.never
    True -> do
      monitor_event <- ReflexUtil.processLines env (Shh.exe "nmcli" "monitor")
      ReflexUtil.performEventThreaded env monitor_event \_ -> do
        CommandUtil.tryCmd (Shh.exe "nmcli" "-g" "name" "connection" "show" "--active")
          <&> decodeUtf8
            % lines
            % filter (/= "lo")
