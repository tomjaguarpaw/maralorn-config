module StatusScript.Modules.Mail (mail, unreadChar) where

import Data.Aeson (FromJSON, eitherDecode')
import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
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
    | Text.isInfixOf "Basecamp" info.authors || Text.isInfixOf "basecamp.com" (fst info.query) =
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
      retryWithBackoff (notmuch "search" "--format=json" "folder:/Inbox/" |> Shh.captureTrim)
        <&> (eitherDecode' >>> either (error . toText) id >>> fmapMaybe (mkWarning mode') >>> sortOn (.heading))
  holdDyn [] events
