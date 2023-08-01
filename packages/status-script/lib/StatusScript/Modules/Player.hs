module StatusScript.Modules.Player (playerModule) where

import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["playerctl"]
missingExecutables :: IO [FilePath]
playerCTLFormat :: String
playerCTLFormat = [i|{{playerName}}@@@{{status}}@@@{{title}} | {{album}} | {{artist}}|]

playerModule :: forall t m. R.MonadHeadlessApp t m => FilePath -> m (R.Event t [PlayerState])
playerModule home = do
  ReflexUtil.reportMissing missingExecutables
  player_event <- ReflexUtil.processLines (playerctl "metadata" "-F" "-f" playerCTLFormat)
  ReflexUtil.performEventThreaded player_event $ const do
    player_states <- playerctl "metadata" "-a" "-f" playerCTLFormat |> Shh.captureTrim
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
