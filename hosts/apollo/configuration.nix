{ config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.

let
  inherit (config.m-0.private) me wireguard;
  inherit (config.m-0) hosts prefix;
  nixos-hardware = (builtins.fetchGit "https://github.com/nixos/nixos-hardware");
in {

imports = [
  "${nixos-hardware}/lenovo/thinkpad"
  "${nixos-hardware}/common/pc/ssd"
  "${(builtins.fetchGit "ssh://git@git.darmstadt.ccc.de/cdark.net/nixdark")}"
 ./hardware-configuration.nix
 ../../system
 ../../system/modules/fonts.nix
];

networking = {
  hostName = "apollo";
  firewall.allowedTCPPorts = [ 8888 ];
  firewall.allowedUDPPorts = [ 30000 ];
  wireguard.interfaces = {
    m0wire = {
      allowedIPsAsRoutes = false;
      ips = [ "${hosts.apollo-wg}/112" ];
      privateKeyFile = "/etc/nixos/hosts/apollo/secret/wireguard-private";
      peers = [
        {
          publicKey = wireguard.pub.hera;
          allowedIPs = [ "::/0" ];
          endpoint = "${hosts.hera-v4}:${builtins.toString wireguard.port}";
          presharedKeyFile = "/etc/nixos/common/secret/wireguard-psk";
          persistentKeepalive = 25;
        }
      ];
      postSetup = [ "${pkgs.iproute}/bin/ip route add ${prefix}::/64 dev m0wire" ];
    };
  };
};

m-0 = {
  laptop.enable = true;
  standalone.enable = true;
};

#let
  #secretsFile = "/var/lib/luks-secret/key";
  #secretsInitrd = "/boot/grub/secrets-initrd.gz";
#in
#{
  #imports = [

    #({lib, config, ...}: lib.mkIf (builtins.pathExists secretsFile) {
      #boot.initrd.luks.devices."root" = {
        #fallbackToPassword = true;
        #keyFile = secretsFile;
      #};
      ## copy the secret into the additional initramfs. `null` means same path
      #boot.initrd.secrets."${secretsFile}" = null;
    #})

    #({lib, config, ...}: lib.mkIf (config.boot.loader.grub.enable && config.boot.initrd.secrets != {}) {
      #boot.loader = {
        #supportsInitrdSecrets = lib.mkForce true;
        #grub.extraInitrd = secretsInitrd;
        #grub.extraPrepareConfig = ''
          #${config.system.build.initialRamdiskSecretAppender}/bin/append-initrd-secrets ${secretsInitrd}
        #'';
      #};
    #})
  #];
#}


# Use the systemd-boot EFI boot loader.
boot = {
	loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/EFI";
      };
		grub = {
			enable = true;
			version = 2;
			device = "nodev";
			efiSupport = true;
			enableCryptodisk = true;
			gfxmodeEfi = "1024x768";
		};

	};
	supportedFilesystems = [ "exfat" ];
};

services = {
  prometheus.exporters.node.firewallFilter = "-i m0wire -p tcp -m tcp --dport 9100";
  autorandr.enable = true;
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
  ed25519PrivateKeyFile = /etc/nixos/hosts + "/${config.networking.hostName}" + /secret/tinc/ed25519_key.priv;
  hostsDirectory = (builtins.fetchGit "ssh://git@git.darmstadt.ccc.de/cdark.net/hosts");
  ip6address = "fd23:42:cda:4342::2";
  ip4address = "172.20.71.2";
};

}
