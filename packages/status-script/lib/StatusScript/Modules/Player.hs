module StatusScript.Modules.Player (playerModule) where

import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["playerctl"]
missingExecutables :: IO [FilePath]
playerCTLFormat :: String
playerCTLFormat = [i|{{playerName}}@@@{{status}}@@@{{title}} | {{album}} | {{artist}}|]

playerModule :: R.MonadHeadlessApp t m => Env -> m (R.Event t [PlayerState])
playerModule = \env -> do
  let home = env.homeDir
  CommandUtil.reportMissing missingExecutables
  player_event <- ReflexUtil.processLines env (playerctl "metadata" "-F" "-f" playerCTLFormat)
  ReflexUtil.performEventThreaded env player_event $ const do
    player_states <- CommandUtil.tryCmd (playerctl "metadata" "-a" "-f" playerCTLFormat)
    mpd_host <-
      [i|#{home}/.config/mpDris2/mpDris2.conf|]
        & readFileBS
        % Exception.try @Exception.IOException
        %> fromRight ""
        %> decodeUtf8
        %> lines
        %> mapMaybe (Text.stripPrefix "host = ")
        %> find (/= "::")
        %> fmap (" " <>)
        %> fromMaybe ""
    pure $
      player_states
        & decodeUtf8
        % Text.lines
        %> Text.splitOn "@@@"
        % mapMaybe
          ( \case
              [name, status, title] ->
                Just $
                  MkPlayerState
                    { name = if name == "mpd" then name <> mpd_host else name
                    , title = cleanTitle title
                    , status = status
                    }
              _ -> Nothing
          )

cleanTitle :: Text -> Text
cleanTitle =
  Text.replace "\"" ""
    % Text.splitOn " "
    % filter (Text.null % not)
    % Text.unwords
    % Text.splitOn "|"
    %> Text.strip
    % filter (Text.null % not)
    % Text.intercalate "\\n"

data PlayerState = MkPlayerState
  { name :: Text
  , status :: Text
  , title :: Text
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
