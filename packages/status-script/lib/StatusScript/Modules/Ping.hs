module StatusScript.Modules.Ping (ping') where

import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
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
ping' :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Warning])
ping' = \env -> do
  reportMissing missingExecutables
  tick <- tickEvent 15
  performEventThreaded env tick \_ -> do
    unreachable_hosts <- flip filterM hosts \host -> isLeft <$> (Shh.tryFailure do (ping "-c" "1" (toString host)) &> Shh.devNull)
    pure $
      unreachable_hosts
        <&> \host ->
          MkWarning
            { description = Just [i|No tunnel to #{host}|]
            , group = "warning"
            , subgroup = Nothing
            }
