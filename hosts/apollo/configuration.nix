{ config, pkgs, ... }:
{

networking = {
	hostName = "apollo";
	networkmanager.enable = true;
	hostId = "38d29f92";
};

i18n.consoleKeyMap = "neo";

imports = [
   <home-manager/nixos>
   ./hardware-configuration.nix
   ../../modules/cdarknet
   ../../host-common/common.nix
];

home-manager.users = {};

# Use the systemd-boot EFI boot loader.
boot = {
	loader = {
		systemd-boot.enable = true;
		efi.canTouchEfiVariables = true;
	};
	supportedFilesystems = [ "exfat" ];
};

security.rngd.enable = true;

cdark_net = {
  enable = true;
  hostName = "maralorn_apollo";
  ed25519PrivateKeyFile = /etc/nixos/local/tinc/ed25519_key.priv;
  hostsDirectory = /etc/nixos/config/modules/cdarknet/hosts;
  ip6address = "fd23:42:cda:4342::2";
  ip4address = "172.20.71.2";
};


hardware.pulseaudio.enable = true;
services = {
#  printing = {
#    enable = true;
#    drivers = [pkgs.hplip];
#  };
#  gnome3 = {
#    gnome-keyring.enable = true;
#    evolution-data-server.enable = true;
#    gnome-disks.enable = true;
#  };
  xserver = {
    enable = true;
    layout = "de";
    xkbVariant = "neo";
    libinput.enable = true;
    desktopManager.gnome3.enable = true;
    displayManager.auto = {
      enable = true;
      user = "maralorn";
    };
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
