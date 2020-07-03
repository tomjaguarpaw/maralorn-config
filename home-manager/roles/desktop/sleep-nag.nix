{ pkgs, ... }:
let
  sleep-nag = pkgs.writeHaskellScript {
    name = "sleep-nag";
    imports = [
      "Data.Time.LocalTime"
      "Data.Time.Format"
      "Data.Time.Clock"
      "Control.Concurrent"
      "Data.Functor"
    ];
    bins = [ pkgs.libnotify ];
  } ''
    main = forever $ do
       time <- getZonedTime
       let tod = localTimeOfDay . zonedTimeToLocalTime$ time
           hour = todHour tod
           night = (hour < 6 || hour >= 23)
           diff = diffUTCTime (zonedTimeToUTC time{zonedTimeToLocalTime = (zonedTimeToLocalTime time){localTimeOfDay = TimeOfDay 23 0 0}}) (zonedTimeToUTC time)
           delay = toRational diff
       if night then (do
         notify_send ([i|Es ist #{formatTime defaultTimeLocale "%H:%M" time} Uhr: Zeit ins Bett zu gehen!|]::String) "Du kannst das hier auch morgen tun!"
         threadDelay 600000000)
       else
         threadDelay (floor $ delay * 1000000)
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
