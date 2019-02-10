{ config, pkgs, lib, ... }:
with lib;
{
options = {
  m-0.server.initSSHKey = mkOption {
    type = types.path;
  };
};

config = mkIf config.m-0.server.enable {
  boot.initrd = {
    network = {
      enable = true;
      ssh = {
        enable = true;
        authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;

        # generate file with
        # nix-shell -p dropbear
        # dropbearkey -t rsa -f boot_rsa
        hostRSAKey = config.m-0.server.initSSHKey;
      };
    };
    postMountCommands = "ip link set eth0 down";
  };
};

}
