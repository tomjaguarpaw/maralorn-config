{ config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.

let
  inherit (config.m-0.private) me;
in {

imports = [
 ./hardware-configuration.nix
 ../../system
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

services = {
  borgbackup.jobs.data = {
    doInit = false;
    encryption.mode = "none";
    paths = "/home/${me.user}/data";
    repo = "borg@borg:.";
    compression = "zstd,5";
  };
};

m-0 = {
  # dropbearkey -t rsa -f /etc/nixos/hosts/<hostname>/secret/boot_rsa
  server.enable = true;
  standalone.enable = true;
  git-server.enable = true;
};

home-manager.users."${me.user}" = (import ./home.nix);

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
