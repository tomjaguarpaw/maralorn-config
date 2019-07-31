{ config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.
# dropbearkey -t rsa -f /etc/nixos/hosts/<hostname>/secret/boot_rsa

let
  inherit (config.m-0.private) me;
in {

imports = [
 ./hardware-configuration.nix
 ../../system
 ../../system/test-timer.nix
 ../../system/standalone.nix
 ../../system/server.nix
 ../../system/git.nix
 ./borg.nix
 ./mail.nix
 ./boot.nix
 ./cloud.nix
 ./web.nix
 ./monitoring.nix
 ./network.nix
 ./matrix.nix
 ./secret
];

nix.sshServe = {
  enable = true;
  keys = me.keys;
  protocol = "ssh-ng";
};

services = {
  borgbackup.jobs.data = {
    doInit = false;
    encryption.mode = "none";
    paths = "/home/${me.user}/data";
    repo = "borg@borg:.";
    compression = "zstd,5";
  };
};

users.users.choreutes = {
      linger = true;
      description = "choreutes";
      isNormalUser = true;
      uid = 1001;
      extraGroups = [ "wheel" "systemd-journal" ];
      passwordFile = "/etc/nixos/hosts/hera/secret/pw-choreutes";
};

# This value determines the NixOS release with which your system is to be
# compatible, in order to avoid breaking some software such as database
# servers. You should change this only after NixOS release notes say you
# should.
system.stateVersion = "18.03"; # Did you read the comment?

}
