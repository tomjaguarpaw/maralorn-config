{pkgs, ...}: let
  calendars = pkgs.privateValue [] "calendars";
  ui = pkgs.writeShellScriptBin "calendar" ''
    ikhal -d Serien
  '';
in {
  home.packages = [pkgs.khal ui];
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
