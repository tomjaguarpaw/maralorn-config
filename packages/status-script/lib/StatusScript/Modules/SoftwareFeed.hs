module StatusScript.Modules.SoftwareFeed (softwareFeed) where

import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
-- import StatusScript.FileWatch qualified as FileWatch

import Shh ((|>))
import Shh qualified
import StatusScript.Env (Env (..))
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil qualified as ReflexUtil
import StatusScript.Warnings (Warning (..))

softwareFeed
  :: R.MonadHeadlessApp t m
  => Env
  -> R.Dynamic t Mode
  -> m (R.Event t [Warning])
softwareFeed = \env _ -> do
  -- db_event <- FileWatch.watchFile env (env.homeDir </> ".local/share/newsboat") "software-updates-cache.db"
  db_event <- (<>) <$> ReflexUtil.tickEvent 3600 <*> R.getPostBuild
  ReflexUtil.performEventThreaded env db_event
    $ const
    $ (Shh.exe ("software-updates") "-x" "print-unread" |> Shh.captureTrim)
    & liftIO
    %> decodeUtf8
    %> Text.replace " unread articles" ""
    %> toString
    %> readMaybe
    %> fromMaybe 0
    %> \case
      0 -> []
      n ->
        [ MkWarning
            { description = Just [i|Code Updates: #{n}|]
            , group = "warning"
            , subgroup = Nothing
            }
        ]
