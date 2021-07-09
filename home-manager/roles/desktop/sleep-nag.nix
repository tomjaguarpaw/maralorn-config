{ pkgs, ... }:
let
  sleep-nag = pkgs.writeHaskellScript
    {
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
           minute = todMin tod
           evening = hour == 0
           night = (hour < 6 && hour >= 1)
           action
            | evening = notify_send "Shutdown alert!" ([i|Rechner fährt in #{59-minute} Minuten runter.|]::String)
            | night = exe "/run/wrappers/bin/sudo" "systemctl" "poweroff"
            | otherwise = pass
       action
       threadDelay 600000000
  '';
in
{
  systemd.user.services.sleep-nag = {
    Unit.Description = "Sleep nag";
    Service.ExecStart = "${sleep-nag}/bin/sleep-nag";
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
