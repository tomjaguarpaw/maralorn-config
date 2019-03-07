{ config, ... }:
let
  inherit (config.m-0) hosts;
in
{
services.prometheus.exporters.node = {
  enable = true;
  openFirewall = true;
  firewallFilter = "! -i ens18 -p tcp -m tcp --dport 9100";
  enabledCollectors = [ "systemd" "logind" ];
};
m-0.monitoring = [
  { name = "hera"; host = "hera-intern.m-0.eu:9100";  }
  { name = "monitoring-container"; host = "localhost:9100"; }
];

containers.monitoring = {
  autoStart = true;
  privateNetwork = true;
  hostBridge = "bridge";
  config = { pkgs, lib, ... }: {
    imports = [
      ../../system
    ];
    networking = {
      interfaces.eth0 = {
        ipv6.addresses = [{ address = hosts.monitoring; prefixLength = 112; }];
      };
      inherit (config.networking) nameservers;
      defaultGateway6 = { address = hosts.hera-intern; interface = "eth0"; };
      firewall.allowedTCPPorts = [ 9090 ];
    };
    services.prometheus = {
      enable = true;
      scrapeConfigs = [
        {
          job_name = "nodes";
          static_configs = map (entry: {
            targets = [ entry.host ];
            labels = {"name" = entry.name; };
          }) config.m-0.monitoring;
        }
      ];
      exporters.node.enable = true;
    };
  };
};

}
