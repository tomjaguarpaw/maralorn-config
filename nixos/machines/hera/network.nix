{
  pkgs,
  config,
  ...
}: let
  #wireguard = import ../../../common/wireguard.nix;
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
    #firewall.allowedUDPPorts = [wireguard.port];
    #wireguard.interfaces = {
    #  m0wire = {
    #    ips = ["${hosts.hera-wg}/112" "${hosts.vpn.hera}/64"];
    #    privateKeyFile = config.age.secrets."wireguard/hera-private".path;
    #    listenPort = wireguard.port;
    #    peers = [
    #      {
    #        publicKey = wireguard.pub.zeus;
    #        allowedIPs = ["${hosts.zeus-wg}/128" "${hosts.vpn.zeus}/128"];
    #        presharedKeyFile = config.age.secrets."wireguard/psk".path;
    #      }
    #      {
    #        publicKey = wireguard.pub.apollo;
    #        allowedIPs = ["${hosts.apollo-wg}/128" "${hosts.vpn.apollo}/128"];
    #        presharedKeyFile = config.age.secrets."wireguard/psk".path;
    #      }
    #      {
    #        publicKey = wireguard.pub.fluffy;
    #        allowedIPs = ["${hosts.vpn.fluffy}/128"];
    #        presharedKeyFile = config.age.secrets."wireguard/psk".path;
    #      }
    #      {
    #        publicKey = wireguard.pub.pegasus;
    #        allowedIPs = ["${hosts.vpn.pegasus}/128"];
    #        presharedKeyFile = config.age.secrets."wireguard/psk".path;
    #      }
    #    ];
    #  };
    #};
  };

  services.ndppd = {
    enable = true;
    configFile = pkgs.writeText "ndppd.conf" ''
      proxy ens18 {
        rule ${config.m-0.prefix}::/64 {
          static
        }
      }
    '';
  };
}
