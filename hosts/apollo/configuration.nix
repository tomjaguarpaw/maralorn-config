{ config, pkgs, ... }:
{

  imports = [
   <home-manager/nixos>
   ./hardware-configuration.nix
   ../../system
];

networking = {
  hostName = "apollo";
};

m-0 = {
  laptop.enable = true;
};

home-manager.users.maralorn = {
  imports = [ ./home.nix ];
};

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
  hostName = "maralorn_apollo";
  ed25519PrivateKeyFile = /etc/nixos/hosts/apollo/secret/tinc/ed25519_key.priv;
  hostsDirectory = /etc/nixos/system/modules/cdarknet/hosts;
  ip6address = "fd23:42:cda:4342::2";
  ip4address = "172.20.71.2";
};

}
