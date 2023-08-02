module StatusScript.Modules.Ping (ping) where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((&>))
import Shh qualified
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))

hosts :: [Text]
hosts = ["hera", "fluffy"]

ping :: R.MonadHeadlessApp t m => m (R.Event t [Warning])
ping = do
  tick <- ReflexUtil.tickEvent 15
  ReflexUtil.performEventThreaded tick \_ -> do
    unreachable_hosts <- flip filterM hosts \host -> isLeft <$> (Shh.tryFailure do (Shh.exe "/run/wrappers/bin/ping" "-c" "1" (toString host)) &> Shh.devNull)
    pure $ unreachable_hosts <&> \host -> MkWarning{description = [i|No tunnel to #{host}|], group = "warning", subgroup = Nothing}
