{
  config,
  pkgs,
  ...
}: let
  commonOptions = {
    enableACME = true;
    forceSSL = true;
    extraConfig = ''
      satisfy any;
      allow ${config.m-0.prefix}::/64;
      allow ${config.m-0.hosts.tailscale.hera.AAAA}/64;
      allow ${config.m-0.hosts.tailscale.hera.A}/24;
      deny all;
    '';
    basicAuthFile = config.age.secrets."basic-auth/monitoring".path;
  };
in {
  imports = [
    ./alertmanager.nix
    ./grafana.nix
    ./prometheus.nix
    ./probes.nix
  ];

  services = {
    nginx = {
      enable = true;
      virtualHosts."alerts.maralorn.de" =
        {
          locations."/".proxyPass = "http://localhost:9093";
        }
        // commonOptions;
      virtualHosts."stats.maralorn.de" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:3000/";
      };
      virtualHosts."monitoring.maralorn.de" =
        {
          locations."/".proxyPass = "http://localhost:9090";
        }
        // commonOptions;
    };
  };
}
