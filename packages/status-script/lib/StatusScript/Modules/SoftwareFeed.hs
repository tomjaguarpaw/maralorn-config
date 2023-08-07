module StatusScript.Modules.SoftwareFeed (softwareFeed) where

import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.FileWatch qualified as FileWatch
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))
import System.Environment qualified as Env
import System.FSNotify qualified as Notify
import System.FilePath ((</>))

softwareFeed ::
  R.MonadHeadlessApp t m =>
  Notify.WatchManager ->
  R.Dynamic t Mode ->
  m (R.Event t [Warning])
softwareFeed = \watch_manager mode -> do
  home <- liftIO $ Env.getEnv "HOME"
  db_event <- FileWatch.watchFile watch_manager (home </> ".local/share/newsboat") "software-updates-cache.db"
  ReflexUtil.performEventThreaded (ReflexUtil.taggedAndUpdated mode db_event) \case
    Code ->
      Shh.exe "software-updates" "-x" "print-unread"
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
