module StatusScript.CommandUtil (tryCmd, reportMissing, loadSystemdEnv) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.List.Extra (firstJust)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Relude
import Say (sayErr)
import Shh ((|>))
import Shh qualified
import System.Environment (setEnv)

Shh.load Shh.Absolute ["systemctl"]

missingExecutables :: IO [FilePath]
tryCmd :: Shh.Proc a -> IO LBS.ByteString
tryCmd x = Shh.ignoreFailure x |> Shh.captureTrim

loadSystemdEnv :: Text -> IO ()
loadSystemdEnv var = do
  reportMissing missingExecutables
  setEnv (toString var)
    =<< maybe (fail [i|#{var} not found in systemd-environment|]) (pure . decodeUtf8)
      . firstJust (LBS.stripPrefix [i|#{var}=|])
      . LBSC.lines
    =<< (systemctl "--user" "show-environment" |> Shh.captureTrim)

reportMissing :: MonadIO m => IO [FilePath] -> m ()
reportMissing missing = whenJustM (nonEmpty <$> liftIO missing) \missing' -> sayErr [i|Missing executables: #{Text.intercalate "," (toList missing' <&> toText)}|]
