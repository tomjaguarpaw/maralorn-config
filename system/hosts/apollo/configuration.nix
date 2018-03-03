{ config, pkgs, ... }:
{

networking = {
	hostName = "apollo";
	networkmanager.enable = true;
	hostId = "38d29f92";
};

i18n.consoleKeyMap = "neo";

imports = [
   ./hardware-configuration.nix
   ../../modules/cdarknet
   ../../snippets/common.nix
];

# Use the systemd-boot EFI boot loader.
boot = {
	loader = {
		systemd-boot.enable = true;
		efi.canTouchEfiVariables = true;
	};
	supportedFilesystems = [ "zfs" "exfat" ];
};

environment.systemPackages = with pkgs; [
	zfstools
	gnome3.caribou
   xournal
];

security.rngd.enable = true;

programs.gnupg.agent = {
	enable = true;
};

cdark_net = {
  enable = true;
  hostName = "maralorn_apollo";
  ed25519PrivateKeyFile = /etc/nixos/local/tinc/ed25519_key.priv;
  hostsDirectory = /etc/nixos/config/modules/cdarknet/hosts;
  ip6address = "fd23:42:cda:4342::2";
  ip4address = "172.20.71.2";
};


services = {
   printing = {
     enable = true;
     drivers = [pkgs.hplip];
   };
	 xserver = {
	 	enable = true;
	 	layout = "de";
	 	xkbVariant = "neo";
	 	#desktopManager.gnome3.enable = true;
      #windowManager.i3.enable = true;
      # displayManager.gdm = {
      #   autoLogin = {
      #     delay = 3;
      #     enable = true;
      #     user = "maralorn";
      #   };
      #   wayland = false;
      #   enable = true;
      # };
		libinput.enable = true;
		config = ''
			Section "InputClass"
			Identifier "Enable libinput for TrackPoint"
			MatchIsPointer "on"
			Driver "libinput"
			EndSection
		'';
	};
};
#virtualisation.docker.enable = true;
}
