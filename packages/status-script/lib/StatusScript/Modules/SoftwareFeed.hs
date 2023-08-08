module StatusScript.Modules.SoftwareFeed (softwareFeed) where

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

softwareFeed ::
  R.MonadHeadlessApp t m =>
  Env ->
  R.Dynamic t Mode ->
  m (R.Event t [Warning])
softwareFeed = \env mode -> do
  db_event <- FileWatch.watchFile env (env.homeDir </> ".local/share/newsboat") "software-updates-cache.db"
  ReflexUtil.performEventThreaded env (ReflexUtil.taggedAndUpdated mode db_event) \case
    Code ->
      Shh.exe (env.homeDir </> ".nix-profile" </> "bin" </> "software-updates") "-x" "print-unread"
        & CommandUtil.tryCmd
        % liftIO
        %> decodeUtf8
        %> Text.replace " unread articles" ""
        %> toString
        %> readMaybe
        %> fromMaybe 0
        %> \case
          0 -> []
          n ->
            [ MkWarning
                { description =
                    Just [i|Code Updates: #{n}|]
                , group = "warning"
                , subgroup = Nothing
                }
            ]
    _ -> pure []
