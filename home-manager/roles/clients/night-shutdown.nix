{ pkgs, lib, ... }:
let
  night-shutdown =
    pkgs.writeHaskellScript
      {
        name = "night-shutdown";
        imports = [
          "Data.Time"
          "Data.Fixed"
          "Control.Concurrent"
          "Data.Functor"
        ];
        bins = [
          pkgs.systemd
          pkgs.set-timer
        ];
      }
      ''
        interval = 5 * 60 -- 5 minutes

        main = forever $ do
           time <- getZonedTime
           let tod = localTimeOfDay . zonedTimeToLocalTime $ time
               hour = todHour tod
               minute = todMin tod
               secs = todSec tod + fromInteger (60 * toInteger minute)
               action
                | hour == 23 && minute >= 30 = systemctl "poweroff"
                | hour >= 22 = set_timer "Shutdown" "23:30"
                | otherwise = set_timer "Shutdown"
           action
           threadDelay $ ceiling $ (interval - (secs `mod'` interval)) * 1000000
      '';
in
{
  systemd.user.services.night-shutdown = {
    Unit.Description = "Night Shutdown";
    Service.ExecStart = lib.getExe night-shutdown;
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
