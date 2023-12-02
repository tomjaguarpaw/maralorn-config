{
  pkgs,
  lib,
  config,
  ...
}:
let
  addressbooks = pkgs.privateValue [] "addressbooks";
  calendars = pkgs.privateValue [] "calendars";
  mkConfig =
    config:
    (pkgs.formats.ini {}).generate "vdirsyncer-config" (
      lib.mapAttrs (_: lib.mapAttrs (_: builtins.toJSON)) config
    );
  mkCalendar =
    {
      name,
      url,
      username,
      passwordPath,
      collections ? [
        "from a"
        "from b"
      ],
      readOnly ? false,
      type ? "caldav",
    }:
    let
      pairName = "${name}_calendar";
      remoteName = "${pairName}_remote";
      localName = "${pairName}_local";
    in
    {
      "pair ${pairName}" = {
        a = localName;
        b = remoteName;
        inherit collections;
        conflict_resolution = "b wins";
        metadata = ["color"];
      };
      "storage ${localName}" = {
        type = "filesystem";
        path = "~/.calendars/${name}/";
        fileext = ".ics";
      };
      "storage ${remoteName}" =
        {
          inherit type;
          inherit url;
        }
        // (
          if (type == "caldav") then
            {
              inherit username;
              "password.fetch" = [
                "command"
                (lib.getExe config.programs.rbw.package)
                "get"
              ] ++ passwordPath;
              read_only = readOnly;
            }
          else
            {}
        );
    };
  mkAddressbook =
    {
      name,
      url,
      username,
      passwordPath,
      collections ? [
        "from a"
        "from b"
      ],
      readOnly ? false,
    }:
    let
      pairName = "${name}_contacts";
      remoteName = "${pairName}_remote";
      localName = "${pairName}_local";
    in
    {
      "pair ${pairName}" = {
        a = localName;
        b = remoteName;
        inherit collections;
        conflict_resolution = "b wins";
      };
      "storage ${localName}" = {
        type = "filesystem";
        path = "~/.contacts/${name}/";
        fileext = ".vcf";
      };
      "storage ${remoteName}" = {
        type = "carddav";
        inherit url username;
        "password.fetch" = [
          "command"
          (lib.getExe config.programs.rbw.package)
          "get"
        ] ++ passwordPath;
        read_only = readOnly;
      };
    };
in
{
  xdg.configFile."vdirsyncer/config".source = mkConfig (
    pkgs.lib.fold (a: b: a // b) {general.status_path = "~/.vdirsyncer/status";} (
      map mkCalendar calendars ++ map mkAddressbook addressbooks
    )
  );
  home.packages = [pkgs.vdirsyncer];

  systemd.user = {
    services.watch-vdir = {
      Unit.Description = "Watch vdir data for changes";
      Service = {
        Environment = "PATH=${lib.makeBinPath [config.programs.rbw.package]}";
        ExecStart = toString (
          pkgs.writeShellScript "watch-vdir" ''
            while ${pkgs.coreutils}/bin/sleep 1s; do
              ${pkgs.vdirsyncer}/bin/vdirsyncer sync
              ${pkgs.inotify-tools}/bin/inotifywait -e move,create,delete,modify -r ${config.home.homeDirectory}/.contacts ${config.home.homeDirectory}/.calendars
            done
          ''
        );
      };
      Install.WantedBy = ["default.target"];
    };
    services.vdirsyncer = {
      Unit.Description = "vdirsyncer sync";
      Service = {
        Type = "oneshot";
        Environment = "PATH=${lib.makeBinPath [config.programs.rbw.package]}";
        ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer sync";
        Restart = "on-failure";
        RestartSec = "1min";
      };
    };
    timers.vdirsyncer = {
      Unit.Description = "vdirsync sync timer";
      Timer.OnCalendar = "*:0/15";
      Install.WantedBy = ["timers.target"];
    };
  };
}
