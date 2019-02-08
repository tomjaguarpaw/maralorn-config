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
 ./network.nix
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
  mathechor-de = {
    enable = true;
    password = config.m-0.private.mathechor-pw;
  };
};

home-manager.users."${me.user}" = (import ./home.nix);

# This value determines the NixOS release with which your system is to be
# compatible, in order to avoid breaking some software such as database
# servers. You should change this only after NixOS release notes say you
# should.
system.stateVersion = "18.03"; # Did you read the comment?

}
