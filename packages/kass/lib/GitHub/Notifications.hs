module GitHub.Notifications where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, eitherDecode')
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Network.Wreq (checkResponse, defaults, header, responseHeader, responseStatus, statusCode)
import Network.Wreq qualified as Wreq
import Network.Wreq.Lens (responseBody)
import Optics
import Relude
import System.Process.Typed (readProcessStdout_)

data Subject = MkSubject
  { title :: Text
  , url :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Notifications = MkNotifications
  { subject :: Subject
  , url :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

getToken :: IO Text
getToken =
  Text.strip . decodeUtf8 <$> readProcessStdout_ "rbw get github.com -f kass"

modifiedFormatString :: String
modifiedFormatString = "%a, %d %b %Y %X GMT"

getNotifications :: Maybe UTCTime -> IO (UTCTime, NominalDiffTime, Maybe [Notifications])
getNotifications start = do
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
            start
      )
      "https://api.github.com/notifications"
  let last_mod =
        response
          ^? pre (foldVL (responseHeader "Last-Modified"))
          % to decodeUtf8
          % to (parseTimeM True defaultTimeLocale modifiedFormatString)
          % _Just
      poll_interval = response ^? pre (foldVL (responseHeader "X-Poll-Interval")) % to decodeUtf8 % to readMaybe % _Just
      nots =
        if response ^. lensVL (responseStatus . statusCode) == 304
          then Nothing
          else either (error . toText) id $ eitherDecode' $ response ^. lensVL responseBody
  pure
    ( fromMaybe (error "Never got a Last-Modified") $ last_mod <|> start
    , fromInteger $ fromMaybe (error "no X-Poll-Interval") poll_interval
    , nots
    )

watchNotifications :: ([Notifications] -> IO ()) -> IO ()
watchNotifications cb = go Nothing
 where
  go prev_last = do
    (last', wait, res) <- getNotifications prev_last
    whenJust res cb
    threadDelay (floor . (* 1_000_000) $ wait)
    go (Just last')
