module StatusScript.Notify (notifyHomeAssistant) where

import Data.Aeson (Options (..), ToJSON (..), defaultOptions, encode, genericToJSON)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Network.Wreq (defaults, header, postWith)
import Network.Wreq qualified as Wreq
import Optics
import Reflex
import Reflex.Host.Headless (MonadHeadlessApp)
import Relude
import StatusScript.Mode
import StatusScript.Modules.Mail
import StatusScript.Modules.Vikunja
import StatusScript.Warnings

data Notification = MkNotification
  { message :: Text
  , data' :: NotificationData
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Notification where
  toJSON = genericToJSON (defaultOptions{fieldLabelModifier = filter (/= '\'')})

data NotificationData = MkNotificationData
  { notification_icon :: Text
  -- ^ e.g. "mdi:list-status" See: https://pictogrammers.com/library/mdi/
  -- | To replace or update
  , tag :: Text
  , subject :: Text
  -- ^ A summary displayed for collapsed message
  , color :: Text
  -- ^ e.g. "red" or "#ff0000"
  , persistent :: Bool
  -- ^ Probably a no-op on modern Android
  , sticky :: Bool
  -- ^ Keep notification when clicked
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

getToken :: IO Text
getToken = Text.strip . decodeUtf8 <$> readFileBS "/run/agenix/home-assistant-token"

wreqOptions :: IO Wreq.Options
wreqOptions = do
  token <- getToken
  pure $ defaults & lensVL (header "Authorization") .~ [[i|Bearer #{token}|]]

mkNotification :: Mode -> [Warning] -> Notification
mkNotification m w =
  MkNotification
    { message =
        Text.unlines $
          warnings <&> \ws ->
            Text.filter (/= taskChar) $
              "<b>"
                <> (head ws).heading
                <> "</b>: "
                <> Text.intercalate " | " (toList $ Text.intercalate "," . (.description) <$> ws)
    , data' =
        MkNotificationData
          { notification_icon
          , tag = "status"
          , subject = ""
          , color = "white"
          , persistent = True
          , sticky = True
          }
    }
 where
  warnings = warningSections w
  notification_icon
    | elemOf (folded % #subgroup % _Just) unreadChar w = "mdi:new-box"
    | m == Sort = "mdi:sort-bool-ascending-variant"
    | elemOf (folded % #heading) "Checklisten" w = "mdi:clipbourd-list-outline"
    | null warnings = "mdi:check-all"
    | otherwise = "mdi:format-list-checks"

notifyHomeAssistant :: MonadHeadlessApp t m => Dynamic t (Mode, [Warning]) -> m ()
notifyHomeAssistant warnings = do
  opts <- liftIO wreqOptions
  notify_ev <- updated <$> holdUniqDyn (uncurry mkNotification <$> warnings)
  performEvent_ $
    void
      . liftIO
      . postWith opts "https://home.maralorn.de/api/services/notify/mobile_app_pegasus"
      . encode
      <$> notify_ev
