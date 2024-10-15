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
import StatusScript.CommandUtil
import StatusScript.Env
import StatusScript.Modules.Mail
import StatusScript.Modules.Vikunja
import StatusScript.ReflexUtil
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

mkNotification :: Bool -> [Warning] -> Notification
mkNotification with_unread w' =
  MkNotification
    { message =
        Text.unlines $
          warnings <&> \ws ->
            Text.filter (/= taskChar) $
              "<b>"
                <> (head ws).heading
                <> ":</b> "
                <> Text.intercalate " <b>|</b> " (foldMap (take 1 . (.description)) ws)
    , data' =
        MkNotificationData
          { notification_icon
          , tag = "status"
          , subject = Text.unwords $ warnings <&> \ws -> (head ws).heading <> " " <> show (length ws)
          , color = "white"
          , persistent = True
          , sticky = True
          }
    }
 where
  warnings = warningSections w
  w
    | with_unread = w'
    | otherwise = filter (hasn't (#subgroup % _Just % only unreadChar)) w'
  notification_icon
    | elemOf (folded % #subgroup % _Just) unreadChar w = "mdi:new-box"
    | elemOf (folded % #heading) "Checklisten" w = "mdi:clipboard-list-outline"
    | null warnings = "mdi:check-all"
    | otherwise = "mdi:format-list-checks"

notifyHomeAssistant :: MonadHeadlessApp t m => Env -> Dynamic t [Warning] -> m ()
notifyHomeAssistant env warnings = do
  opts <- liftIO wreqOptions
  forM_ [("pegasus", True), ("kalliope", False)] \(device, with_unread) -> do
    notify_ev <- updated <$> holdUniqDyn (mkNotification with_unread <$> warnings)
    performEventThreaded env notify_ev $ liftIO . sendNotification opts device

sendNotification :: Wreq.Options -> Text -> Notification -> IO ()
sendNotification opts device n = do
  void $
    retryTimeout 2 5 $
      postWith opts [i|https://home.maralorn.de/api/services/notify/mobile_app_#{device}|] d
 where
  d = encode n
