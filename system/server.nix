{ config, pkgs, lib, ... }:
{

imports = [ ./init_ssh.nix ];

config = {
  systemd.services."system-maintenance" = {
    startAt = "2:45";
    environment.NIX_PATH = "/etc/nix-path:nixos-config=/etc/nixos/configuration.nix";
    path = [ pkgs.git ];
    restartIfChanged = false;
    unitConfig.X-StopOnRemoval = false;
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${(import ./update-lib.nix config.system.build.nixos-rebuild).system-maintenance}/bin/system-maintenance";
    };
  };
};

}
