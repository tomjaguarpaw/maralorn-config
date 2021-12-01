{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.signald;

in
{
  options = {
    services.signald = {
      enable = mkEnableOption "Signald, an unofficial daemon for interacting with Signal";

      socketFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = "/var/run/signald/signald.sock";
        description = ''
          When started, signald will create a unix socket at this location. To
          interact with it, connect to that socket and send new line (\n)
          terminated JSON strings.
        '';
      };
    };
  };


  config = mkIf cfg.enable {
    users.users."signald" = { isSystemUser = true; group = "signald"; };
    users.groups."signald" = { };
    systemd.tmpfiles.rules = [ "Z /var/lib/signald 0770 signald signald - -" ];

    systemd.services.signald = {
      description = "A daemon for interacting with the Signal Private Messenger";

      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];

      serviceConfig = {
        Type = "simple";
        WatchdogSignal = "SIGTERM";
        WatchdogSec = "60m";
        Restart = "always";

        PermissionsStartOnly = true;
        RuntimeDirectory = "signald";

        ProtectSystem = "full";
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;

        DynamicUser = false;
        Group = "signald";
        User = "signald";
        StateDirectory = "signald";
        UMask = 0007;


        ExecStart = ''
          ${pkgs.signald}/bin/signald \
            ${optionalString (cfg.socketFile != null) "--socket ${cfg.socketFile}"} \
            --data=''${STATE_DIRECTORY} \
            --database=jdbc:sqlite:''${STATE_DIRECTORY}/signald.db
        '';
      };
    };
  };

  meta.maintainers = with maintainers; [ expipiplus1 ];
}

