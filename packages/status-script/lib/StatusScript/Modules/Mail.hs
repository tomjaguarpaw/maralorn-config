module StatusScript.Modules.Mail (mail, unreadChar) where

import Data.Aeson (FromJSON, eitherDecode')
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.Clock (UTCTime (..))
import Optics hiding ((|>))
import Reflex
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Relude hiding (catMaybes)
import Shh ((|>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil
import StatusScript.Warnings
import System.FilePath ((</>))
import Witherable (catMaybes)

Shh.load Shh.Absolute ["notmuch"]

missingExecutables :: IO [FilePath]

data NotmuchInfo = MkNotmuchInfo
  { matched :: Int
  , total :: Int
  , authors :: Text
  , subject :: Text
  , date_relative :: Text
  , query :: (Text, Maybe Text)
  , tags :: Seq Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON)

mkWarning :: Mode -> NotmuchInfo -> Maybe Warning
mkWarning mode info
  | mode == Sort || isUnread =
      Just $
        MkWarning
          { description = [fst (Text.breakOn "|" info.authors) <> ": " <> info.subject]
          , heading
          , barDisplay = Count
          , group = group'
          , subgroup = if isUnread then Just unreadChar else Nothing
          }
  | otherwise = Nothing
 where
  isUnread = "unread" `elem` info.tags
  (heading, group')
    | Text.isInfixOf "Basecamp" info.authors
        || Text.isInfixOf "sent you a Ping" info.subject
        || Text.isInfixOf "basecamp.com" (fst info.query) =
        ("HS Chat", toEnum 0xf1309) -- nf-md-account_tie_voice_outline
    | Text.isInfixOf "heilmannsoftware" (fst info.query) = ("HS GitHub", toEnum 0xf10ca) -- nf-md-account_tie_outline
    | Text.isInfixOf "github.com" (fst info.query) = ("GitHub", toEnum 0xea84) -- nf-cod-github
    | otherwise = ("E-Mail", toEnum 0xeb1c) -- nf-cod-mail

unreadChar :: Char
unreadChar = toEnum 0xf0394 -- nf-md-new_box

mail :: R.MonadHeadlessApp t m => Env -> R.Dynamic t Mode -> m (Dynamic t [Warning])
mail env mode = do
  CommandUtil.reportMissing missingExecutables
  notmuch_update <- FileWatch.watchFile env (env.homeDir </> "Maildir/.notmuch/xapian") "flintlock"
  events <- performEventThreaded env (taggedAndUpdated mode notmuch_update) \case
    DND -> pure mempty
    mode' ->
      retryIndefinite 5 (notmuch "search" "--format=json" "folder:/Inbox/" |> Shh.captureTrim)
        <&> (eitherDecode' >>> either (error . toText) id >>> fmapMaybe (mkWarning mode') >>> sortOn (.heading))
  warnings <- holdDyn [] events
  set_timeout_ev <- performEvent $ updated $ zipDynWith calcTimeout mode warnings
  next_timeout <- foldDyn ($) (Just $ UTCTime (toEnum 0) 0) set_timeout_ev
  let warning_throttle =
        zipDynWith
          ( maybe
              (const $ pure Nothing)
              \next w -> liftIO getCurrentTime <&> \now' -> if now' >= next then (Just w) else Nothing
          )
          next_timeout
          warnings
  tick_minutes <- tickEvent 60
  holdDyn [] . catMaybes
    =<< performEvent (leftmost [updated warning_throttle, tag (current warning_throttle) tick_minutes])

calcTimeout :: MonadIO m => Mode -> [Warning] -> m (Maybe UTCTime -> Maybe UTCTime)
calcTimeout m ws = do
  now' <- liftIO getCurrentTime
  pure case m of
    _ | null ws -> (const Nothing)
    Normal -> minMaybe $ flip addUTCTime now' <$> timeouts ws
    _ -> minMaybe (Just now')
 where
  minMaybe = maybe id (\new -> Just . maybe new (min new))

timeouts :: [Warning] -> Maybe NominalDiffTime
timeouts = fmap timeout >>> maximumOf folded

timeout :: Warning -> NominalDiffTime
timeout = (.heading) >>> headingTimeout

headingTimeout :: Text -> NominalDiffTime
headingTimeout = \case
  "HS Chat" -> 30 * minute
  "HS GitHub" -> 60 * minute
  _ -> 120 * minute

minute :: NominalDiffTime
minute = 60
