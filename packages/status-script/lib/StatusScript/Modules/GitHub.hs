module StatusScript.Modules.GitHub (notifications) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwIO)
import Data.ByteString.Char8 qualified as Bytestring
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import GitHub (Auth (..), FetchCount (..), Notification (..), Subject (..), URL, getNotificationsR, getUrl, github)
import GitHub.Internal.Prelude (Vector)
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
      (toList <$> r)

parseIssueUrl :: URL -> Maybe (Text, Maybe Char, Text)
parseIssueUrl =
  getUrl >>> Text.stripPrefix "https://api.github.com/repos/" >=> Text.splitOn "/" >>> \case
    [org, repo, type', id'] -> Just ([i|#{org}/#{repo}|], typeChar type', id')
    _ -> Nothing

typeChar :: Text -> Maybe Char
typeChar = \case
  "issues" -> Just $ toEnum 0xeb0c -- nf-cod-issues
  "pulls" -> Just $ toEnum 0xf04c2 -- nf-md-source_pull
  _ -> Nothing

mkWarning :: Notification -> Warning
mkWarning n =
  MkWarning
    { description = Just description
    , group = toEnum 0xf02a4 -- nf-md-github
    , subgroup
    }
 where
  title = n.notificationSubject.subjectTitle
  (description, subgroup) = case parseIssueUrl n.notificationSubject.subjectURL of
    Nothing -> (title, Nothing)
    Just (repo, icon, num) -> ([i|#{repo} \##{num} #{title}|], icon)

getToken :: IO ByteString
getToken =
  Bytestring.strip . toStrict <$> (Shh.exe "rbw" "get" "github.com" "-f" "kass" Shh.|> Shh.captureTrim)

watchNotifications :: (Vector Notification -> IO ()) -> IO ()
watchNotifications cb = forever $ do
  token <- getToken
  response <- github (OAuth token) (getNotificationsR FetchAll)
  either throwIO cb response
  threadDelay 60_000_000 -- one minute
