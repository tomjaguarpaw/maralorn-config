module StatusScript.CommandUtil (reportMissing, loadSystemdEnv, retryWithBackoff) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (catchAny)
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
retryWithBackoff :: IO a -> IO a
retryWithBackoff act = go 0
 where
  go cnt = do
    catchAny act \err -> do
      sayErr [i|Retrying in #{timeout} ms. Error: #{displayException err}|]
      threadDelay (timeout * 1_000)
      go (cnt + 1)
   where
    timeout = min 60_000 (2 ^ cnt)

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
