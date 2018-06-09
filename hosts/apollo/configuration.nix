{ config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.

let
  me = config.m-0.private.me;
in {

imports = [
  <nixos-hardware/lenovo/thinkpad>
  <nixos-hardware/common/pc/ssd>
 ./hardware-configuration.nix
 ../../system
 ./syncthing.nix
];

networking = {
  hostName = "apollo";
};

m-0 = {
  laptop.enable = true;
  standalone.enable = true;
};

home-manager.users."${me.user}" = (import ./home.nix);

# Use the systemd-boot EFI boot loader.
boot = {
	loader = {
		systemd-boot.enable = true;
		efi.canTouchEfiVariables = true;
	};
	supportedFilesystems = [ "exfat" ];
};

services = {
  borgbackup.jobs.data = {
    doInit = false;
    startAt = [];
    exclude = [ "/home/${me.user}/data/media" ];
    encryption.mode = "none";
    paths = "/home/${me.user}/data";
    repo = "borg@borg:.";
    compression = "zstd,22";
  };
};

cdark_net = {
  enable = true;
  hostName = "${me.user}_${config.networking.hostName}";
  ed25519PrivateKeyFile = builtins.toPath "/etc/nixos/hosts/${config.networking.hostName}/secret/tinc/ed25519_key.priv";
  hostsDirectory = /etc/nixos/system/modules/cdarknet/hosts;
  ip6address = "fd23:42:cda:4342::2";
  ip4address = "172.20.71.2";
};



}
