{ pkgs, lib, ... }:
{
  systemd.user.services.status-script = {
    Unit.Description = "Status Script";
    Service = {
      Environment = "PATH=${lib.makeBinPath [ pkgs.nix ]}";
      ExecStart = lib.getExe pkgs.status-script;
      Restart = "always";
      RestartSec = "10s";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
