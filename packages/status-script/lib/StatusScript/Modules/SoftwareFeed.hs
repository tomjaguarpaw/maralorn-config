module StatusScript.Modules.SoftwareFeed (softwareFeed) where

import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Reflex
import Reflex.Host.Headless (MonadHeadlessApp)
import Relude
import Shh (ignoreFailure, (|>))
import Shh qualified
import StatusScript.Env (Env (..))
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil
import StatusScript.Warnings

softwareFeed
  :: MonadHeadlessApp t m
  => Env
  -> Dynamic t Mode
  -> m (Dynamic t [Warning])
softwareFeed = \env mode_dyn -> do
  performDynThreaded env mode_dyn [] $ \case
    Sort -> getUpdates
    _ -> pure []

getUpdates :: MonadIO m => m ([Warning])
getUpdates =
  liftIO $ parseWarning <$> (ignoreFailure (Shh.exe ("software-updates") "-x" "print-unread") |> Shh.captureTrim)
 where
  parseWarning = mkWarn . fromMaybe 0 . readMaybe . toString . Text.replace " unread articles" "" . decodeUtf8
  mkWarn = \case
    0 -> []
    n ->
      [ MkWarning
          { description = [[i|Code Updates: #{n}|]]
          , heading = "Updates"
          , barDisplay = Count
          , group = toEnum 0xf4af -- nf-oct-code_review
          , subgroup = Nothing
          }
      ]
