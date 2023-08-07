module StatusScript.Modules.Mail (mail) where

import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.Environment qualified as Env
import System.FSNotify qualified as Notify
import System.FilePath ((</>))

Shh.load Shh.Absolute ["notmuch"]
missingExecutables :: IO [FilePath]
processNotmuchDescription :: Text -> Text
processNotmuchDescription =
  Text.splitOn "["
    % drop 1
    % Text.intercalate "["
    % ("[" <>)
    % Text.splitOn "("
    % reverse
    % drop 1
    % reverse
    % Text.intercalate "("
    % Text.replace "\"" ""

mkWarning :: Text -> Text -> Warning
mkWarning = \subgroup msg ->
  MkWarning
    { description = Just (processNotmuchDescription msg)
    , group = "inbox"
    , subgroup = Just subgroup
    }

mail :: R.MonadHeadlessApp t m => Notify.WatchManager -> R.Dynamic t Mode -> m (R.Event t [Warning])
mail watch_manager mode = do
  CommandUtil.reportMissing missingExecutables
  home <- liftIO $ Env.getEnv "HOME"
  notmuch_update <-
    FileWatch.watchFile watch_manager (home </> "Maildir/.notmuch/xapian") "flintlock"
  events <-
    [ ((/= Klausur), ["folder:hera/Inbox", "tag:unread"], "e-mail")
      , ((== Orga), ["folder:hera/Inbox", "not", "tag:unread"], "e-mail-open")
      , ((== Code), ["folder:hera/Code"], "Code")
      ]
      & mapM \(on_mode, folder, subgroup) ->
        ReflexUtil.performEventThreaded
          (ReflexUtil.taggedAndUpdated mode notmuch_update)
          \case
            mode' | on_mode mode' -> Text.lines . decodeUtf8 <$> (notmuch "search" folder |> Shh.captureTrim)
            _ -> pure []
          <<&>> fmap (mkWarning subgroup)
  ReflexUtil.concatEvents events
