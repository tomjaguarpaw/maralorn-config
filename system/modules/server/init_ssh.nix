{ config, pkgs, lib, ... }:
with lib;
{

config = mkIf config.m-0.server.enable {
  boot.initrd = {
    network = {
      enable = true;
      ssh = {
        enable = true;
        authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;

        # generate file with
        # dropbearkey -t rsa -f /etc/nixos/boot_rsa
        # nix-env -iA nixos.dropbear
        hostRSAKey = builtins.toPath "/etc/nixos/hosts/${config.networking.hostName}/secret/boot_rsa";
      };
    };
    postMountCommands = "ip link set eth0 down";
  };
};

}
