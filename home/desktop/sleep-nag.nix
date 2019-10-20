{ lib, pkgs, config, ... }:
let
  inherit (import ../../lib) writeHaskellScript;
  inherit (import ../../pkgs) eventd;
  sleep-nag = writeHaskellScript {
    name = "sleep-nag";
    imports = [
      "Data.Time.LocalTime"
      "Control.Concurrent"
      "Data.Functor"
    ];
    bins = [ eventd ];
  } ''
    main = go
      where
        go = do
          tod <- getZonedTime <&> localTimeOfDay . zonedTimeToLocalTime
          when (todHour tod < 6 || todHour tod >= 23) $ eventc "notification" "kassandra" "-d" ([i|title='Es ist #{todHour tod}:#{todMin tod} Uhr: Zeit ins Bett zu gehen!'|]::String) "-d" "message='Du kannst das hier auch morgen tun!'"
          threadDelay 600000000
          go
  '';
in {

  systemd.user = {
    services.sleep-nag = {
      Unit = { Description = "Sleep nag"; };
      Service = { ExecStart = "${sleep-nag}/bin/sleep-nag"; };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };

}
