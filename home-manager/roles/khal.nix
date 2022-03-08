{pkgs, ...}: let
  calendars = pkgs.privateValue [] "calendars";
  plans = pkgs.privateValue
  {
    "workDay" = "pass";
    "weekend" = "pass";
  }
  "plans";
  planning = pkgs.writeShellScriptBin "planning" ''
    create-plans; kassandra2
  '';
  ui = pkgs.writeShellScriptBin "calendar" ''
    create-plans; ikhal -d Serien
  '';
  createPlans = pkgs.writeHaskellScript
  {
    name = "create-plans";
    bins = [pkgs.khal pkgs.vdirsyncer];
    imports = [
      "Data.Time"
    ];
  } ''
    main = do
      today <- localDay . zonedTimeToLocalTime <$> getZonedTime
      [0..7] & fmap (`addDays` today) & mapM_ \day -> do
        planned <- khal ["list", "-a", "Planung", show day, "06:00", "24h", "--notstarted"] |> captureTrim
        when (planned == "No events") $ do
          say $ "Creating events for " <> show day
          if (dayOfWeek day `elem` [Saturday, Sunday]) then do
            ${plans.weekend}
          else do
            ${plans.workDay}
  '';
in {
  home.packages = [pkgs.khal createPlans planning ui];
  xdg.configFile."khal/config".text = ''
    [locale]
    dateformat = "%Y-%m-%d"
    datetimeformat = "%Y-%m-%d %H:%M"
    timeformat = "%H:%M"
    [default]
    default_calendar = Standard
    [calendars]
    ${
      pkgs.lib.concatMapStringsSep "\n" (
        {
          name,
          readOnly ? false,
          ...
        }: ''
          [[${name}]]
          type = discover
          path = ~/.calendars/${name}/*
          readonly = ${
            if readOnly
            then "True"
            else "False"
          }''
      )
      calendars
    }
  '';
}
