{ pkgs, lib, ... }:
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
          pkgs.systemd
          pkgs.set-timer
        ];
      }
      ''
        interval = 5 * 60 * 1000000 -- 5 minutes

        main = forever $ do
           time <- getZonedTime
           let tod = localTimeOfDay . zonedTimeToLocalTime$ time
               hour = todHour tod
               minute = todMin tod
               secs = (todSec tod + fromInteger (60 * toInteger minute)) * 1000000
               evening = hour > 22
               night = (hour == 23 && minute >= 30)
               action
                | evening = set_timer "Shutdown" "23:30"
                | night = systemctl "poweroff"
                | otherwise = set_timer "Shutdown"
           action
           threadDelay $ interval - (floor secs `mod` interval)
      '';
in
{
  systemd.user.services.night-shutdown = {
    Unit.Description = "Night Shutdown";
    Service.ExecStart = lib.getExe night-shutdown;
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
