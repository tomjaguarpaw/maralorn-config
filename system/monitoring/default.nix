{ config, ... }:
let
  inherit (config.m-0.private) monitoring-guest-pw monitoring-pw;
  commonOptions = {
    enableACME = true;
    forceSSL = true;
    extraConfig = ''
      satisfy any;
      allow ${config.m-0.prefix}::/64;
      deny all;
    '';
    basicAuth = {
      guest = monitoring-guest-pw;
      maralorn = monitoring-pw;
    };
  };
in {
  imports = [
    ./alertmanager.nix
    ./grafana.nix
    ./prometheus.nix
    ./probes.nix
    ./nixpkgs.nix
  ];

  services = {
    nginx = {
      enable = true;
      virtualHosts."alerts.maralorn.de" = {
        locations."/".proxyPass = "http://localhost:9093";
      } // commonOptions;
      virtualHosts."stats.maralorn.de" = {
        locations."/".proxyPass = "http://localhost:3000/";
      } // commonOptions;
      virtualHosts."monitoring.maralorn.de" = {
        locations."/".proxyPass = "http://localhost:9090";
      } // commonOptions;
    };
  };

}
