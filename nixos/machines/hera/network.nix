{ pkgs, config, ... }:
let
  inherit (config.m-0) hosts;
  inherit (config.m-0.private) wireguard;
in {
  networking = {
    hostName = "hera";
    interfaces.ens18 = {
      proxyARP = true;
      ipv4.addresses = [{
        address = "213.136.94.190";
        prefixLength = 24;
      }];
      ipv6.addresses = [{
        address = hosts.hera;
        prefixLength = 128;
      } {
        address = hosts.hera-wg-host;
        prefixLength = 128;
      }];
    };
    defaultGateway = "213.136.94.1";
    defaultGateway6 = {
      address = "fe80::1";
      interface = "ens18";
    };

    firewall = {
      extraCommands = ''
        ip6tables -A INPUT -s ${config.m-0.prefix}::/64 -j ACCEPT
        ip6tables -A FORWARD -p ipv6-icmp -j ACCEPT
        ip6tables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
        ip6tables -A FORWARD ! -s ${config.m-0.prefix}::/64 -j DROP
      '';
    };

    bridges.bridge.interfaces = [ ];
    interfaces.bridge = {
      proxyARP = true;
      ipv6.addresses = [{
        address = hosts.hera-intern;
        prefixLength = 112;
      }];
      ipv4.addresses = [{
        address = "10.0.0.1";
        prefixLength = 24;
      }];
    };
    nat = {
      enable = true;
      externalInterface = "ens18";
      internalInterfaces = [ "bridge" ];
    };
    nameservers = [ "213.136.95.10" "2a02:c207::1:53" "2a02:c207::2:53" ];
    firewall.allowedTCPPorts = [ 8666 ];
    firewall.allowedUDPPorts = [ wireguard.port ];
    wireguard.interfaces = {
      m0wire = {
        ips = [ "${hosts.hera-wg}/112" ];
        privateKeyFile = "/etc/nixos/nixos/machines/hera/secret/wireguard-private";
        listenPort = wireguard.port;
        peers = [{
          publicKey = wireguard.pub.apollo;
          allowedIPs = [ "${hosts.apollo-wg}/128" ];
          presharedKeyFile = "/etc/nixos/common/secret/wireguard-psk";
        }];
      };
    };
  };
  services = {
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
