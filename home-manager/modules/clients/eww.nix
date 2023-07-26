{
  pkgs,
  lib,
  config,
  ...
}:
{

  systemd.user.services.eww = {
    Unit = {
      X-Restart-Triggers = [ "${config.programs.eww.configDir}" ];
      Description = "EWW";
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
            config.programs.rbw.package
          ]
        }";
      ExecStart = "${lib.getExe config.programs.eww.package} daemon --no-daemonize";
      ExecStartPost = "${lib.getExe config.programs.eww.package} open bar";
      Restart = "always";
      RestartSec = "10s";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
  programs.eww = {
    enable = true;
    package = pkgs.callPackage ./_eww-package.nix { withWayland = true; };
    configDir = ./eww-config;
  };
}
