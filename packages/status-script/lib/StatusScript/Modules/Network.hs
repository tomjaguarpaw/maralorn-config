module StatusScript.Modules.Network where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["nmcli"]
missingExecutables :: IO [FilePath]
networkState :: (R.MonadHeadlessApp t m) => Env -> m (R.Event t [Text])
networkState = \env -> do
  monitor_event <- ReflexUtil.processLines env (nmcli "monitor")
  ReflexUtil.performEventThreaded env monitor_event \_ -> do
    CommandUtil.tryCmd (nmcli "-g" "name" "connection" "show" "--active")
      <&> decodeUtf8
        % lines
        % filter (/= "lo")
