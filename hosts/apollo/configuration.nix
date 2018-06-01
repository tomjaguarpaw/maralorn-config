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
];

networking = {
  hostName = "apollo";
};

m-0 = {
  laptop.enable = true;
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

cdark_net = {
  enable = true;
  hostName = "${me.user}_${config.networking.hostName}";
  ed25519PrivateKeyFile = builtins.toPath "/etc/nixos/hosts/${config.networking.hostName}/secret/tinc/ed25519_key.priv";
  hostsDirectory = /etc/nixos/system/modules/cdarknet/hosts;
  ip6address = "fd23:42:cda:4342::2";
  ip4address = "172.20.71.2";
};

services = {
  mpd = {
      enable = true;
      user = me.user;
      group = "users";
      network.listenAddress = "::0";
      musicDirectory = "/home/${me.user}/data/aktuell/media/musik";
  };
};


}
