{
  config,
  pkgs,
  ...
}: let
  commonOptions = {
    enableACME = true;
    forceSSL = true;
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
      virtualHosts."stats.maralorn.de" =
        {
          locations."/".proxyPass = "http://localhost:3000/";
        }
        // commonOptions;
      virtualHosts."monitoring.maralorn.de" =
        {
          locations."/".proxyPass = "http://localhost:9090";
        }
        // commonOptions;
    };
  };
}
