{
  pkgs,
  lib,
  config,
  ...
}:
{

  systemd.user.services.status-script = {
    Unit = {
      Description = "status-script";
    };
    Service = {
      Environment = "PATH=${
          lib.makeBinPath [
            pkgs.coreutils
            pkgs.hyprland
            pkgs.jq
            pkgs.socat
            pkgs.bash
            pkgs.status-script
            pkgs.curl
            pkgs.openssh
            pkgs.gitMinimal
            pkgs.nix
            pkgs.taskwarrior
            config.programs.rbw.package
          ]
        }";
      ExecStart = lib.getExe pkgs.status-script;
      Restart = "always";
      RestartSec = "10s";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
