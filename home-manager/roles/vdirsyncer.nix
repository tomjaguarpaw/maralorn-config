{ pkgs, lib, ... }:
let
  addressbooks = pkgs.privateValue [] "addressbooks";
  calendars = pkgs.privateValue [] "calendars";
  mkConfig = config:
    (pkgs.formats.ini {}).generate "vdirsyncer-config" (
      lib.mapAttrs
        (
          name: section:
            (lib.mapAttrs (name: option: builtins.toJSON option) section)
        ) config
    );
  mkCalendar = { name, url, username, passwordPath, collections ? [ "from a" "from b" ], readOnly ? false, type ? "caldav" }:
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
        "storage ${remoteName}" = {
          inherit type;
          inherit url;
        } // (if (type == "caldav") then {
          inherit username;
          "password.fetch" = [ "command" "${pkgs.pass}/bin/pass" passwordPath ];
          read_only = readOnly;
        } else {});
      };
  mkAddressbook = { name, url, username, passwordPath, collections ? [ "from a" "from b" ], readOnly ? false }:
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
          "password.fetch" = [ "command" "${pkgs.pass}/bin/pass" passwordPath ];
          read_only = readOnly;
        };
      };
in
{
  home = {
    packages = [ pkgs.vdirsyncer ];
    file.".config/vdirsyncer/config".source = mkConfig
      (
        pkgs.lib.fold (a: b: a // b) {
          general.status_path = "~/.vdirsyncer/status";
        } (map mkCalendar calendars ++ map mkAddressbook addressbooks)
      );
  };

  systemd.user = {
    services.vdirsyncer = {
      Unit.Description = "vdirsyncer sync";
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer sync";
      };
    };
    timers.vdirsyncer = {
      Unit.Description = "vdirsync sync timer";
      Timer.OnCalendar = "*:0/15";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
