module StatusScript.Modules.SoftwareFeed (softwareFeed) where

import Data.Text qualified as Text
import Maralorn.Prelude
-- import StatusScript.FileWatch qualified as FileWatch

import Reflex
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.Env (Env (..))
import StatusScript.Mode (Mode (..))
import StatusScript.ReflexUtil
import StatusScript.Warnings (Warning (..))

softwareFeed
  :: R.MonadHeadlessApp t m
  => Env
  -> R.Dynamic t Mode
  -> m (Dynamic t [Warning])
softwareFeed = \env mode_dyn -> do
  -- db_event <- FileWatch.watchFile env (env.homeDir </> ".local/share/newsboat") "software-updates-cache.db"
  pb <- getPostBuild
  let trigger_event = void (updated mode_dyn) <> pb
  ev <-
    performEventThreaded env trigger_event $
      const $
        (Shh.exe ("software-updates") "-x" "print-unread" |> Shh.captureTrim)
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
                    , group = toEnum 61729
                    , subgroup = Nothing
                    }
                ]
  val <- holdDyn [] ev
  pure $
    zipDynWith
      ( \case
          Sort -> id
          _ -> const []
      )
      mode_dyn
      val
