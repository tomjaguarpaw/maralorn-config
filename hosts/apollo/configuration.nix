{ config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.

let
  inherit (config.m-0) hosts prefix private;
  inherit (private) me wireguard;
  nixos-hardware = (import ../../nix/sources.nix).nixos-hardware;
in {

  imports = [
    "${nixos-hardware}/lenovo/thinkpad"
    "${nixos-hardware}/common/pc/ssd"
    "${(builtins.fetchGit "ssh://git@git.darmstadt.ccc.de/cdark.net/nixdark")}"
    ./hardware-configuration.nix
    ../../system
    ../../system/fonts.nix
    ../../system/boot-key.nix
    ../../system/standalone
    ../../system/use-cache.nix
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
        peers = [{
          publicKey = wireguard.pub.hera;
          allowedIPs = [ "::/0" ];
          endpoint = "${hosts.hera-v4}:${builtins.toString wireguard.port}";
          presharedKeyFile = "/etc/nixos/common/secret/wireguard-psk";
          persistentKeepalive = 25;
        }];
        postSetup =
          [ "${pkgs.iproute}/bin/ip route add ${prefix}::/64 dev m0wire" ];
      };
    };
  };

  m-0 = { laptop.enable = true; };

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
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint pkgs.hplip ];
    };
    prometheus.exporters.node = {
      firewallFilter = "-i m0wire -p tcp -m tcp --dport 9100";
      openFirewall = true;
    };
    autorandr.enable = true;
  };

  cdark_net = {
    enable = true;
    hostName = "${me.user}_${config.networking.hostName}";
    ed25519PrivateKeyFile = /etc/nixos/hosts + "/${config.networking.hostName}"
      + /secret/tinc/ed25519_key.priv;
    hostsDirectory =
      (builtins.fetchGit "ssh://git@git.darmstadt.ccc.de/cdark.net/hosts");
    ip6address = "fd23:42:cda:4342::2";
    ip4address = "172.20.71.2";
  };
}
