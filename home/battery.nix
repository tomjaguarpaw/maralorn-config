{ lib, pkgs, config, ... }:
let
  inherit (import ../lib/default.nix) writeHaskellScript;
  battery-watch = writeHaskellScript {
    name = "battery-watch";
    libraries = [
      pkgs.haskellPackages.fdo-notify
      pkgs.haskellPackages.megaparsec
      pkgs.haskellPackages.replace-megaparsec
    ];
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
  } ''
    criticalLevel = 20 -- percent
    minutes = 60 * 1000 * 1000 -- threadDelay takes microseconds

    main = do
     client <- connectSession
     let loop = \handleMay -> do
          batteryStateText <- decodeUtf8 <$> (acpi "-a" |> captureTrim)
          batteryLevelText <- decodeUtf8 <$> (acpi "-b" |> captureTrim)
          chargerOnline <- maybe (fail "Couldn‘t get charging state") pure $ parseMaybe onlineParser batteryStateText
          batteryLevel <- maybe (fail "Couldn‘t get battery level") pure $ parseMaybe levelParser batteryLevelText
          let NextAction { note = noteMay , delay = delay } = chooseAction chargerOnline batteryLevel
          handle <- if | Just note <- noteMay -> Just <$> maybe (notify client note) (flip (replace client) note) handleMay
                       | otherwise -> pure handleMay
          echo ([i|Waiting for #{delay} minutes until next message.|] :: String)
          threadDelay $ delay * minutes
          loop handle
     loop Nothing

    data NextAction = NextAction { note :: Maybe Note, delay :: Int }

    type Parser = Parsec Text LT.Text

    onlineParser :: Parser Bool
    onlineParser = not . null . rights <$> sepCap (string "on-line")

    levelParser :: Parser Int
    levelParser = (maybe (fail "No Number found") pure . listToMaybe . rights) =<< sepCap (decimal <* "%")

    chooseAction :: Bool -> Int -> NextAction
    chooseAction chargerOnline batteryLevel
      | chargerOnline = NextAction Nothing 1
      | batteryLevel <= criticalLevel = mkMsg $ myNote {summary = "Battery is low!"}
      | otherwise = mkMsg $ myNote {summary = "Battery is discharging!"}
     where
      mkMsg = flip NextAction (max 1 (batteryLevel `div` 5)) . Just
      myNote = blankNote { body = Just $ Text [i|Only #{batteryLevel}% remaining.|]}
  '';
in {

  systemd.user = {
    services.battery = {
      Unit = { Description = "Watch battery state and warn user"; };
      Service = {
        ExecStart = "${battery-watch}/bin/battery-watch";
        Restart = "always";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };

}
