module StatusScript.Modules.GitHub (notifications) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, eitherDecode')
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Network.Wreq (checkResponse, defaults, header, responseHeader, responseStatus, statusCode)
import Network.Wreq qualified as Wreq
import Network.Wreq.Lens (responseBody)
import Optics
import Reflex
import Reflex.Host.Headless qualified as R
import Relude
import Shh qualified
import StatusScript.Env
import StatusScript.Mode
import StatusScript.Warnings

notifications :: R.MonadHeadlessApp t m => Env -> Dynamic t Mode -> m (Dynamic t [Warning])
notifications env dMode = do
  (event, trigger) <- newTriggerEvent
  liftIO $ env.fork "watchNotifications" (watchNotifications trigger)
  r <- holdDyn mempty $ event <&> fmap mkWarning
  pure $
    zipDynWith
      (\mode -> if mode >= Normal then id else const [])
      dMode
      r

mkWarning :: Notification -> Warning
mkWarning n =
  MkWarning
    { description = Just n.subject.title
    , group = toEnum 0xf02a4 -- nf-md-github
    , subgroup = Nothing
    }

data Subject = MkSubject
  { title :: Text
  , url :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Notification = MkNotification
  { subject :: Subject
  , url :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

getToken :: IO Text
getToken =
  Text.strip . decodeUtf8 <$> (Shh.exe "rbw" "get" "github.com" "-f" "kass" Shh.|> Shh.captureTrim)

modifiedFormatString :: String
modifiedFormatString = "%a, %d %b %Y %X GMT"

getNotifications :: Maybe UTCTime -> Maybe NominalDiffTime -> IO (Maybe UTCTime, NominalDiffTime, Maybe [Notification])
getNotifications last_modified old_poll_interval = do
  whenJust old_poll_interval $ threadDelay . floor . (* 1_000_000)
  token <- getToken
  response <-
    Wreq.getWith
      ( defaults
          & lensVL (header "Authorization") .~ [[i|Bearer #{token}|]]
          --          & lensVL (param "all") .~ ["true"]
          & lensVL checkResponse ?~ (\_ _ -> pure ())
          & maybe
            id
            ( \ts ->
                lensVL (header "If-Modified-Since")
                  .~ [encodeUtf8 (formatTime defaultTimeLocale modifiedFormatString ts)]
            )
            last_modified
      )
      "https://api.github.com/notifications"
  let new_last_modified =
        response
          ^? pre (foldVL (responseHeader "Last-Modified"))
          % to decodeUtf8
          % to (parseTimeM True defaultTimeLocale modifiedFormatString)
          % _Just
      poll_interval = response ^? pre (foldVL (responseHeader "X-Poll-Interval")) % to decodeUtf8 % to readMaybe % _Just
      not_modified = response ^. lensVL (responseStatus . statusCode) == 304
  pure
    ( if not_modified then last_modified else new_last_modified
    , fromMaybe 60 $ (fromInteger <$> poll_interval) <|> old_poll_interval
    , if not_modified
        then Nothing
        else either (error . toText) id $ eitherDecode' $ response ^. lensVL responseBody
    )

watchNotifications :: ([Notification] -> IO ()) -> IO ()
watchNotifications cb = go Nothing Nothing
 where
  go prev_last prev_interv = do
    (last', interv', res) <- getNotifications prev_last prev_interv
    whenJust res cb
    go last' (Just interv')
