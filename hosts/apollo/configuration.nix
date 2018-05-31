{ config, pkgs, ... }:

let
  me = config.m-0.private.me;
in {

imports = [
 <home-manager/nixos>
 ./secret
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
  ed25519PrivateKeyFile = /etc/nixos/hosts/apollo/secret/tinc/ed25519_key.priv;
  hostsDirectory = /etc/nixos/system/modules/cdarknet/hosts;
  ip6address = "fd23:42:cda:4342::2";
  ip4address = "172.20.71.2";
};

services = {
  mpd = {
      enable = true;
      network.listenAddress = "::0";
      musicDirectory = "/home/maralorn/data/aktuell/media/musik";
  };
};


}
