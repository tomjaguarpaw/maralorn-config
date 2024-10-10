module StatusScript.Modules.Ping (ping') where

import Maralorn.Prelude
import Reflex
import Reflex.Host.Headless (MonadHeadlessApp)
import Shh ((&>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.Env
import StatusScript.ReflexUtil
import StatusScript.Warnings

hosts :: [Text]
hosts = ["hera", "athene"]

Shh.load Shh.Absolute ["ping"]

missingExecutables :: IO [FilePath]
ping' :: MonadHeadlessApp t m => Env -> m (Dynamic t [Warning])
ping' = \env -> do
  reportMissing missingExecutables
  tick <- tickEvent 15
  ev <- performEventThreaded env tick \_ -> do
    unreachable_hosts <- flip filterM hosts \host -> isLeft <$> (Shh.tryFailure do (ping "-c" "1" (toString host)) &> Shh.devNull)
    pure $
      unreachable_hosts
        <&> \host ->
          MkWarning
            { description = [[i|No tunnel to #{host}|]]
            , heading = "Ping"
            , barDisplay = Count
            , group = toEnum 983387
            , subgroup = Nothing
            }
  holdDyn [] ev
