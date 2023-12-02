{config, ...}:
let
  inherit (config.m-0) hosts;
in
{
  boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = 1;
  m-0.tailscale-routes = "fd42:ccc:da:64::/64,172.20.64.0/24";
  systemd.network.networks."10-wan" = {
    matchConfig.Name = "ens18";
    address = [
      "${hosts.hera.A}/24"
      "${hosts.hera.AAAA}/128"
    ];
    routes = [
      {routeConfig.Gateway = "213.136.94.1";}
      {routeConfig.Gateway = "fe80::1";}
    ];
  };
  networking = {
    nftables.ruleset = ''
      table ip nixos-nat {
      	chain pre {
      		type nat hook prerouting priority dstnat; policy accept;
      	}

      	chain post {
      		type nat hook postrouting priority srcnat; policy accept;
      		iifname "bridge" oifname "ens18" masquerade comment "from internal interfaces"
      		iifname "tailscale0" oifname "tinc.cdark.net" masquerade comment "snat queries to hackspace"
      	}

      	chain out {
      		type nat hook output priority mangle; policy accept;
      	}
      }
      table ip6 nixos-nat {
      	chain pre {
      		type nat hook prerouting priority dstnat; policy accept;
      	}

      	chain post {
      		type nat hook postrouting priority srcnat; policy accept;
      		iifname "tailscale0" oifname "tinc.cdark.net" masquerade comment "snat queries to hackspace"
      	}

      	chain out {
      		type nat hook output priority mangle; policy accept;
      	}
      }
    '';
    hostName = "hera";
    domain = "m-0.eu";
    nameservers = [
      "213.136.95.10"
      "2a02:c207::1:53"
      "2a02:c207::2:53"
    ];
    firewall.allowedTCPPorts = [8666];
  };
}
