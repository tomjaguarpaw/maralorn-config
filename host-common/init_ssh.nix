{ config, pkgs, ... }:

{
  boot.initrd.network = {
    enable = true;
    ssh = {
      enable = true;
      authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;

      # generate file with
      # dropbearkey -t rsa -f /etc/nixos/boot_rsa
      # nix-env -iA nixos.dropbear
      hostRSAKey = /etc/nixos/local/boot_rsa;
    };
  };
}
