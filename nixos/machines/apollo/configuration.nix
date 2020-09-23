{ lib, config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.

let
  inherit (config.m-0) hosts prefix private;
  inherit (private) me wireguard;
  nixos-hardware = (import ../../../nix/sources.nix).nixos-hardware;
  inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
in {

  imports = [
    "${nixos-hardware}/lenovo/thinkpad"
    "${nixos-hardware}/common/pc/ssd"
    "${(builtins.fetchGit "ssh://git@git.darmstadt.ccc.de/cdark.net/nixdark")}"
    ./hardware-configuration.nix
    ../../roles
    ../../roles/fonts.nix
    ../../roles/boot-key.nix
    ../../roles/standalone
    ../../roles/use-cache.nix
  ];

  networking = {
    hostName = "apollo";
    firewall.allowedUDPPorts = [ 30000 ];
    wireguard.interfaces = {
      m0wire = {
        allowedIPsAsRoutes = false;
        ips = [ "${hosts.apollo-wg}/112" ];
        privateKeyFile =
          "/etc/nixos/nixos/machines/apollo/secret/wireguard-private";
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

  services = {
    beesd.filesystems.root = {
      spec = "LABEL=root";
      hashTableSizeMB = 128;
      verbosity = "crit";
      extraOptions = [ "--loadavg-target" "4.0" ];
    };
    snapper = {
      configs.home = {
        subvolume = "/home";
        extraConfig = ''
          TIMELINE_MIN_AGE="3600"
          TIMELINE_LIMIT_WEEKLY="4"
          TIMELINE_LIMIT_MONTHLY="1"
          TIMELINE_LIMIT_YEARLY="0"
          TIMELINE_CREATE="yes"
          TIMELINE_CLEANUP="yes"
        '';
      };
      cleanupInterval = "15m";
      snapshotInterval = "*:00/3:00";
    };
    prometheus.exporters.node = {
      firewallFilter = "-i m0wire -p tcp -m tcp --dport 9100";
      openFirewall = true;
    };
    syncthing = {
      enable = true;
      group = "users";
      user = "maralorn";
      openDefaultPorts = true;
      declarative = syncthing.declarativeWith [ "hera" ] "/home/maralorn/media"
        // {
          cert = "/etc/nixos/nixos/machines/apollo/secret/syncthing/cert.pem";
          key = "/etc/nixos/nixos/machines/apollo/secret/syncthing/key.pem";
        };
    };
    gnome3.chrome-gnome-shell.enable = true;
    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome3.enable = true;
    };
  };
  boot.kernel.sysctl = { "fs.inotify.max_user_watches" = 204800; };

  #cdark_net = {
    #enable = true;
    #hostName = "${me.user}_${config.networking.hostName}";
    #ed25519PrivateKeyFile = /etc/nixos/nixos/machines
      #+ "/${config.networking.hostName}" + /secret/tinc/ed25519_key.priv;
    #hostsDirectory =
      #pkgs.fetchgit { url = "ssh://git@git.darmstadt.ccc.de/cdark.net/hosts"; };
    #ip6address = "fd23:42:cda:4342::2";
    #ip4address = "172.20.71.2";
  #};
  system.stateVersion = "19.09";
}
