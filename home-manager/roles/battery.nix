{ pkgs, ... }:
let
  battery-watch =
    pkgs.writeHaskellScript
      {
        name = "battery-watch";
        bins = [ pkgs.acpi ];
        imports = [
          "DBus.Notify"
          "Control.Concurrent"
          "Text.Megaparsec"
          "Text.Megaparsec.Char"
          "Text.Megaparsec.Char.Lexer"
          "Replace.Megaparsec"
          "Data.Maybe"
        ];
      }
      ''
        moderateLevel = 50 -- percent
        lowLevel = 20 -- percent
        criticalLevel = 8 -- percent
        minute = 60 * 1000 * 1000 -- threadDelay takes microseconds

        main = do
         client <- connectSession
         let loop = \lastState handleMay -> do
              newState <- getState
              let noteMay = chooseAction lastState newState
              handle <- if | Just note <- noteMay -> Just <$> maybe (notify client note) (flip (replace client) note) handleMay
                           | otherwise -> pure handleMay
              threadDelay $ minute `div` 4
              loop newState handle
         loop (BatState True 100) Nothing

        data BatState = BatState { charging :: Bool, level :: Int }

        getState = do
          batteryStateText <- decodeUtf8 <$> (acpi "-a" |> captureTrim)
          batteryLevelText <- decodeUtf8 <$> (acpi "-b" |> captureTrim)
          chargerOnline <- maybe (fail "Couldn‘t get charging state") pure $ parseMaybe onlineParser batteryStateText
          batteryLevel <- maybe (fail "Couldn‘t get battery level") pure $ parseMaybe levelParser batteryLevelText
          pure $ BatState chargerOnline batteryLevel

        type Parser = Parsec Text LText

        onlineParser :: Parser Bool
        onlineParser = not . null . rights <$> sepCap (string "on-line")

        levelParser :: Parser Int
        levelParser = (maybe (fail "No Number found") pure . listToMaybe . rights) =<< sepCap (decimal <* "%")

        chooseAction :: BatState -> BatState -> Maybe Note
        chooseAction (BatState wasCharging lastLevel) (BatState isCharging currentLevel)
          | wasCharging && isCharging = Nothing
          | wasCharging && not isCharging = Just $ myNote{summary = "Charger disconnected." }
          | not wasCharging && isCharging = Just $ myNote{summary = "Charger connected.", expiry = Milliseconds 5000 }
          | currentLevel <= criticalLevel = Just $ myNote{summary = "Battery is very low!" }
          | currentLevel <= lowLevel && currentLevel < lastLevel = Just $ myNote{summary = "Battery is low!"}
          | ((currentLevel `mod` 5 == 0 && currentLevel <= moderateLevel) || (currentLevel `mod` 10 == 0)) && currentLevel < lastLevel = Just $ myNote{summary = "Battery is discharging."}
          | otherwise = Nothing
         where
          myNote = blankNote { body = Just $ Text [i|#{currentLevel}% remaining.|]}
      ''
  ;
in
{
  systemd.user = {
    services.battery = {
      Unit.Description = "Watch battery state and warn user";
      Service = {
        ExecStart = "${battery-watch}/bin/battery-watch";
        Restart = "always";
        RestartSec = 60;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
