{ lib, config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.

let
  wireguard = import ../../../common/wireguard.nix;
  inherit (config.m-0) hosts prefix;
  nixos-hardware = (import ../../../nix/sources.nix).nixos-hardware;
  inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
in {

  imports = [
    "${nixos-hardware}/lenovo/thinkpad/t480s"
    ./hardware-configuration.nix
    ../../roles
    ../../roles/fonts.nix
    ../../roles/boot-key.nix
    ../../roles/standalone
  ];

  networking = {
    hostName = "apollo";
    firewall.allowedUDPPorts = [ 30000 ];
    wireguard.interfaces = {
      m0wire = {
        allowedIPsAsRoutes = false;
        ips = [ "${hosts.apollo-wg}/112" ];
        privateKeyFile = pkgs.privatePath "wireguard/apollo-private";
        peers = [{
          publicKey = wireguard.pub.hera;
          allowedIPs = [ "::/0" ];
          # endpoint =
          #  "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
          # If v6 is not available:
          endpoint = "[${hosts.hera-v4}]:${builtins.toString wireguard.port}";
          presharedKeyFile = pkgs.privatePath "wireguard/psk";
          persistentKeepalive = 25;
        }];
        postSetup =
          [ "${pkgs.iproute}/bin/ip route add ${prefix}::/96 dev m0wire" ];
      };
    };
  };

  m-0.laptop.enable = true;

  programs.sway.enable = true;

  services = {
    fstrim.enable = true;
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
          cert = pkgs.privatePath "syncthing/apollo/cert.pem";
          key = pkgs.privatePath "syncthing/apollo/key.pem";
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

  system.stateVersion = "19.09";
}
