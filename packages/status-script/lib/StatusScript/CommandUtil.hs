module StatusScript.CommandUtil (reportMissing, loadSystemdEnv, retryIndefinite, retryTimeout) where

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

-- | Retries starting at 1ms with exponential backoff to the given limit.
retryIndefinite
  :: Int
  -- ^ Retry maximally every _ seconds
  -> IO a
  -> IO a
retryIndefinite max_sec act = either absurd id <$> retry (const Nothing) max_sec 0 act

{- | Retries starting at 1ms with exponential backoff to the given limit.
| Fails with last exception after given timeout.
-}
retryTimeout
  :: Int
  -- ^ Retry maximally every _ seconds
  -> Int
  -- ^ Give up after _ seconds
  -> IO a
  -> IO (Maybe a)
retryTimeout max_sec timeout_sec act = either (const Nothing) Just <$> retry Just max_sec timeout_sec act

retry :: (SomeException -> Maybe b) -> Int -> Int -> IO a -> IO (Either b a)
retry f max_sec timeout_sec act = go 0
 where
  go cnt = catchAny (Right <$> act) \err -> do
    case f err of
      Just b | waited_sec > timeout_sec -> do
        sayErr [i|Giving up after #{waited_sec} s. Error: #{displayException err}|]
        pure (Left b)
      _ -> do
        sayErr [i|Retrying in #{timeout_milli cnt} ms. Error: #{displayException err}|]
        threadDelay (timeout_milli cnt * 1_000)
        go (cnt + 1)
   where
    timeout_milli c = min (1000 * max_sec) (2 ^ c)
    waited_sec = sum (timeout_milli <$> [0 .. cnt - 1]) `div` 1000

loadSystemdEnv :: Text -> IO ()
loadSystemdEnv var = do
  reportMissing missingExecutables
  setEnv (toString var)
    =<< maybe (fail [i|#{var} not found in systemd-environment|]) (pure . decodeUtf8)
      . firstJust (LBS.stripPrefix [i|#{var}=|])
      . LBSC.lines
    =<< (systemctl "--user" "show-environment" |> Shh.captureTrim)

reportMissing :: MonadIO m => IO [FilePath] -> m ()
reportMissing missing = whenJustM
  (nonEmpty <$> liftIO missing)
  \missing' -> sayErr [i|Missing executables: #{Text.intercalate "," (toList missing' <&> toText)}|]
