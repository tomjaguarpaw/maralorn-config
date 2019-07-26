{ config, pkgs, lib, ... }:
{

imports = [ ./init_ssh.nix ];

config = {
  systemd.services."system-maintenance" = {
    startAt = "2:45";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${(import ./update-lib.nix config.system.build.nixos-rebuild).system-maintenance}/bin/system-maintenance";
    };
  };
};

}
