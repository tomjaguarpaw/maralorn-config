{ config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.

let
  inherit (config.m-0.private) me wireguard;
  inherit (config.m-0) hosts;
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
  wireguard.interfaces = {
    wireguard = {
      ips = [ "${hosts.apollo}/64" ];
      privateKeyFile = "/etc/nixos/hosts/apollo/secret/wireguard-private";
      peers = [
        {
          publicKey = wireguard.pub.hera;
          allowedIPs = [ "${hosts.hera}/64" ];
          endpoint = "${hosts.hera-v4}:${builtins.toString wireguard.port}";
          presharedKeyFile = "/etc/nixos/common/secret/wireguard-psk";
        }
      ];
    };
  };
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
    exclude = [
      "/home/${me.user}/data/aktuell/media"
      "/home/${me.user}/data/.stversions"
    ];
    encryption.mode = "none";
    paths = "/home/${me.user}/data";
    repo = "borg@borg:.";
    compression = "zstd,5";
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
