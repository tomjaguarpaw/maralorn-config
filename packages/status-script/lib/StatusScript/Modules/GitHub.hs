module StatusScript.Modules.GitHub (notifications) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, eitherDecode')
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import Network.Wreq (defaults, header, responseHeader)
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

parseIssueUrl :: Text -> Maybe (Text, Maybe Char, Text)
parseIssueUrl =
  Text.stripPrefix "https://api.github.com/repos/" >=> Text.splitOn "/" >>> \case
    [org, repo, type', id'] -> Just ([i|#{org}/#{repo}|], typeChar type', id')
    _ -> Nothing

typeChar :: Text -> Maybe Char
typeChar = \case
  "pulls" -> Just $ toEnum 0xf41b -- nf-oct-issue_opened
  "issues" -> Just $ toEnum 0xf04c2 -- nf-md-source_pull
  _ -> Nothing

mkWarning :: Notification -> Warning
mkWarning n =
  MkWarning
    { description = Just description
    , group = toEnum 0xf02a4 -- nf-md-github
    , subgroup
    }
 where
  title = n.subject.title
  (description, subgroup) = case parseIssueUrl =<< n.subject.url of
    Nothing -> (title, Nothing)
    Just (repo, icon, num) -> ([i|#{repo} \##{num} #{title}|], icon)

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

getNotifications :: Maybe NominalDiffTime -> IO (NominalDiffTime, [Notification])
getNotifications old_poll_interval = do
  whenJust old_poll_interval $ threadDelay . floor . (* 1_000_000)
  token <- getToken
  response <-
    Wreq.getWith
      (defaults & lensVL (header "Authorization") .~ [[i|Bearer #{token}|]])
      "https://api.github.com/notifications"
  let
    poll_interval = response ^? pre (foldVL (responseHeader "X-Poll-Interval")) % to decodeUtf8 % to readMaybe % _Just
  pure
    ( fromMaybe 60 $ (fromInteger <$> poll_interval) <|> old_poll_interval
    , either (error . toText) id $ eitherDecode' $ response ^. lensVL responseBody
    )

watchNotifications :: ([Notification] -> IO ()) -> IO ()
watchNotifications cb = go Nothing
 where
  go prev_interv = do
    (interv', res) <- getNotifications prev_interv
    cb res
    go (Just interv')
