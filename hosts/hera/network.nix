{ pkgs, config, ... }:
  let
    inherit (config.m-0) hosts;
    inherit (config.m-0.private) wireguard;
  in
{
networking = {
  hostName = "hera";
  interfaces.ens18 = {
    proxyARP = true;
    ipv4.addresses = [{ address = "213.136.94.190"; prefixLength = 24; }];
    ipv6.addresses = [{ address = hosts.hera; prefixLength = 128; }];
  };
  defaultGateway = "213.136.94.1";
  defaultGateway6 = { address = "fe80::1"; interface = "ens18"; };

  bridges.bridge.interfaces = [ ];
  interfaces.bridge = {
    proxyARP = true;
    ipv6.addresses = [{ address = hosts.hera-intern; prefixLength = 112; }];
  };
  nameservers = [ "213.136.95.10" "2a02:c207::1:53" "2a02:c207::2:53" ];
  firewall.allowedUDPPorts = [ wireguard.port ];
  wireguard.interfaces = {
    m0wire = {
      ips = [ "${hosts.hera-wg}/112" ];
      privateKeyFile = "/etc/nixos/hosts/hera/secret/wireguard-private";
      listenPort = wireguard.port;
      peers = [
        {
          publicKey = wireguard.pub.apollo;
          allowedIPs = [ "${hosts.apollo-wg}/128" ];
          presharedKeyFile = "/etc/nixos/common/secret/wireguard-psk";
        }
      ];
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
