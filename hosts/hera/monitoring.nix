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
          static_configs = [{targets = [ "localhost:9100" "${hosts.hera-intern}:9100" ];}];
        }
      ];
      exporters.node.enable = true;
    };
  };
};

}
