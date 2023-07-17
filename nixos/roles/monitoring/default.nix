{ config, ... }:
let
  inherit (config.m-0) virtualHosts;
in
{
  imports = [
    ./alertmanager.nix
    ./grafana.nix
    ./prometheus.nix
    ./probes.nix
  ];

  services = {
    nginx = {
      enable = true;
      virtualHosts = {
        ${virtualHosts.alerts}.locations."/".proxyPass = "http://localhost:9093";
        ${virtualHosts.stats}.locations."/".proxyPass = "http://localhost:3000/";
        ${virtualHosts.monitoring}.locations."/".proxyPass = "http://localhost:9090";
      };
    };
  };
}
