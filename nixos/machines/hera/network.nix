{
  pkgs,
  config,
  lib,
  ...
}: let
  wireguard = import ../../../common/wireguard.nix;
  inherit (config.m-0) hosts;
in {
  boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = 1;
  networking = {
    hostName = "hera";
    domain = "m-0.eu";
    interfaces.ens18 = {
      proxyARP = true;
      ipv4.addresses = [
        {
          address = "213.136.94.190";
          prefixLength = 24;
        }
      ];
      ipv6.addresses = [
        {
          address = hosts.hera;
          prefixLength = 128;
        }
        {
          address = hosts.hera-wg-host;
          prefixLength = 128;
        }
      ];
    };
    defaultGateway = "213.136.94.1";
    defaultGateway6 = {
      address = "fe80::1";
      interface = "ens18";
    };

    bridges.bridge.interfaces = [];
    interfaces.bridge = {
      proxyARP = true;
      ipv6.addresses = [
        {
          address = hosts.hera-intern;
          prefixLength = 112;
        }
      ];
      ipv4.addresses = [
        {
          address = "10.0.0.1";
          prefixLength = 24;
        }
      ];
    };
    nat = {
      enable = true;
      externalInterface = "ens18";
      internalInterfaces = ["bridge"];
    };
    nameservers = ["213.136.95.10" "2a02:c207::1:53" "2a02:c207::2:53"];
    firewall.allowedTCPPorts = [8666];
    firewall.allowedUDPPorts = [wireguard.port];
    wireguard.interfaces = {
      m0wire = {
        ips = ["${hosts.hera-wg}/112" "${hosts.vpn.hera}/64"];
        privateKeyFile = config.age.secrets."wireguard/hera-private".path;
        listenPort = wireguard.port;
        peers = [
          {
            publicKey = wireguard.pub.zeus;
            allowedIPs = ["${hosts.zeus-wg}/128" "${hosts.vpn.zeus}/128"];
            presharedKeyFile = config.age.secrets."wireguard/psk".path;
          }
          {
            publicKey = wireguard.pub.apollo;
            allowedIPs = ["${hosts.apollo-wg}/128" "${hosts.vpn.apollo}/128"];
            presharedKeyFile = config.age.secrets."wireguard/psk".path;
          }
          {
            publicKey = wireguard.pub.fluffy;
            allowedIPs = ["${hosts.vpn.fluffy}/128"];
            presharedKeyFile = config.age.secrets."wireguard/psk".path;
          }
          {
            publicKey = wireguard.pub.pegasus;
            allowedIPs = ["${hosts.vpn.pegasus}/128"];
            presharedKeyFile = config.age.secrets."wireguard/psk".path;
          }
        ];
      };
    };
  };

  services = {
    unbound = {
      resolveLocalQueries = false;
      enable = true;
      settings = {
        server = {
          access-control = [
            "127.0.0.0/8 allow"
            "::1/128 allow"
            "fdc0:7::/64 allow"
            "100.64.7.0/24 allow"
            "fd7a:115c:a1e0:77::/64 allow"
          ];
          interface = [
            "lo"
            "m0wire"
            "tailscale0"
          ];
          local-data = let
            aliases = with hosts.tailscale; {
              home = fluffy;
              rss = hera;
              monitoring = hera;
              alerts = hera;
            };
          in
            lib.concatLists
            (lib.mapAttrsToList
              (
                name:
                  lib.mapAttrsToList
                  (type: ip: "\"${name}.maralorn.de IN ${type} ${ip}\"")
              )
              aliases);
        };
      };
    };
    ndppd = {
      enable = true;
      configFile = pkgs.writeText "ndppd.conf" ''
        proxy ens18 {
          rule ${config.m-0.prefix}::/64 {
            static
          }
        }
      '';
    };
  };
}
