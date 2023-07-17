{ pkgs, ... }:
let
  night-shutdown =
    pkgs.writeHaskellScript
      {
        name = "night-shutdown";
        imports = [
          "Data.Time.LocalTime"
          "Data.Time.Format"
          "Data.Time.Clock"
          "Control.Concurrent"
          "Data.Functor"
        ];
        bins = [
          pkgs.libnotify
          pkgs.systemd
        ];
      }
      ''
        interval = 5

        main = forever $ do
           time <- getZonedTime
           let tod = localTimeOfDay . zonedTimeToLocalTime$ time
               hour = todHour tod
               minute = todMin tod
               evening = hour == 0
               night = (hour < 6 && hour >= 1)
               action
                | evening = notify_send "Shutdown alert!" ([i|Rechner fährt in #{59-minute} Minuten runter.|]::String)
                | night = systemctl "poweroff"
                | otherwise = pass
           action
           threadDelay $ (interval - (minute `mod` interval)) * 60 * 1000000
      '';
in
{
  systemd.user.services.night-shutdown = {
    Unit.Description = "Night Shutdown";
    Service.ExecStart = "${night-shutdown}/bin/night-shutdown";
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
