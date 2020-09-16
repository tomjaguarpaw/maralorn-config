{ config, pkgs, lib, ... }:
with lib; {
  options = { m-0.server.initSSHKey = mkOption { type = types.path; }; };

  config = {
    boot.initrd = {
      network = {
        enable = true;
        ssh = {
          enable = true;
          authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;
          hostKeys = [ config.m-0.server.initSSHKey ];
        };
      };
      postMountCommands = "ip link set eth0 down";
    };
  };

}
