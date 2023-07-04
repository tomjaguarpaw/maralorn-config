{
  pkgs,
  lib,
  ...
}:
let
  plans =
    pkgs.privateValue
      {
        "workDay" = "pass";
        "weekend" = "pass";
      }
      "plans"
  ;
  createPlans =
    pkgs.writeHaskellScript
      {
        name = "create-plans";
        bins = [
          pkgs.khal
          pkgs.vdirsyncer
        ];
        imports = [ "Data.Time" ];
      }
      ''
        main = do
          today <- localDay . zonedTimeToLocalTime <$> getZonedTime
          [0..7] & fmap (`addDays` today) & mapM_ \day -> do
            planned <- khal ["list", "-a", "Planung", show day, "06:00", "24h", "--notstarted"] |> captureTrim
            when (LBS.null planned) $ do
              say $ "Creating events for " <> show day
              if (dayOfWeek day `elem` [Saturday, Sunday]) then do
                ${plans.weekend}
              else do
                ${plans.workDay}
      ''
  ;
  sync = "${lib.getExe pkgs.vdirsyncer} sync nextcloud_calendar/planung";
in
{
  systemd.user = {
    services.create-plans = {
      Unit.Description = "Create planning appointments in calendar";
      Service = {
        Type = "oneshot";
        ExecStart =
          (pkgs.writeShellScript "update-plans" (
            lib.concatStringsSep "\n" [
              "set -e"
              sync
              (lib.getExe createPlans)
            ]
          )).outPath;
        Restart = "on-failure";
        RestartSec = 60;
      };
    };
    timers.create-plans = {
      Unit.Description = "Create planning appointments in calendar";
      Timer.OnCalendar = "00:01:00";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
