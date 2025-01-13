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
        interval = 5

        main = forever $ do
           time <- getZonedTime
           let tod = localTimeOfDay . zonedTimeToLocalTime$ time
               hour = todHour tod
               minute = todMin tod
               evening = hour > 22
               night = (hour == 23 && minute >= 30)
               action
                | evening = set_timer "Shutdown" "23:30"
                | night = systemctl "poweroff"
                | otherwise = set_timer "Shutdown"
           action
           threadDelay $ (interval - (minute `mod` interval)) * 60 * 1000000
      '';
in
{
  systemd.user.services.night-shutdown = {
    Unit.Description = "Night Shutdown";
    Service.ExecStart = lib.getExe night-shutdown;
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
