module StatusScript.Modules.Mail (mail) where

import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
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

mail :: R.MonadHeadlessApp t m => Env -> R.Dynamic t Mode -> m (R.Event t [Warning])
mail env mode = do
  CommandUtil.reportMissing missingExecutables
  notmuch_update <-
    FileWatch.watchFile env (env.homeDir </> "Maildir/.notmuch/xapian") "flintlock"
  events <-
    [ (const True, ["tag:unread and (not folder:/Archiv/ or folder:/Inbox/)"], "e-mail")
      , (const True, ["folder:hera/Inbox", "not", "tag:unread"], "e-mail-open")
      , (const True, ["folder:hera/Code"], "Code")
      ]
      & mapM \(on_mode, folder, subgroup) ->
        ReflexUtil.performEventThreaded
          env
          (ReflexUtil.taggedAndUpdated mode notmuch_update)
          \case
            mode'
              | on_mode mode' ->
                  CommandUtil.tryCmd (notmuch "search" folder)
                    <&> decodeUtf8
                    % Text.lines
                    %> mkWarning subgroup
            _ -> pure []
  ReflexUtil.concatEvents events
