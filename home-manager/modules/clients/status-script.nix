{
  pkgs,
  lib,
  config,
  ...
}:
{

  options.status-script.env = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [ ];
    description = lib.mdDoc "Packages in the status-script environment.";
  };

  config = {
    status-script.env = [
      pkgs.coreutils
      pkgs.hyprland
      pkgs.jq
      pkgs.socat
      pkgs.bash
      pkgs.status-script
      pkgs.curl
      pkgs.gitMinimal
      pkgs.nix
      pkgs.taskwarrior
      config.programs.rbw.package
    ];
    systemd.user.services.status-script = {
      Unit.Description = "status-script";
      Service = {
        Environment = "PATH=${lib.makeBinPath config.status-script.env}";
        ExecStart = lib.getExe pkgs.status-script;
        Restart = "always";
        RestartSec = "10s";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
