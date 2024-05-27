{ config, lib, ... }:
let
  inherit (config.m-0) hosts;
  inherit (config.networking) hostName;
in
{
  services = {
    prometheus.exporters = {
      nginx = {
        inherit (config.services.nginx) enable;
      };
    };
    nginx = {
      enable = lib.mkDefault (config.m-0.virtualHosts != { });
      virtualHosts = lib.mapAttrs' (name: hostname: {
        name = hostname;
        value = {
          forceSSL = true;
          enableACME = true;
          extraConfig = lib.mkIf (!(builtins.elem name (hosts.publicAliases.${hostName} or [ ]))) ''
            satisfy any;
            ${lib.concatMapStringsSep "\n" (ip_range: "allow ${ip_range};") config.m-0.headscaleIPs}
            deny all;
          '';
        };
      }) config.m-0.virtualHosts;
      statusPage = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedTlsSettings = true;
      clientMaxBodySize = "500m";
    };
  };

  systemd.services =
    let
      hosts = builtins.attrNames config.services.nginx.virtualHosts;
      makeConfig = host: {
        name = "acme-${host}";
        value = {
          # Let's Encrypt Failed Validation Limit allows 5 retries per hour, per account, hostname and hour.
          serviceConfig = {
            Restart = "on-failure";
            RestartSec = lib.mkForce "2m"; # Upstream sets 900s but does not change StartLimit
          };
          unitConfig = {
            StartLimitIntervalSec = "10m";
            StartLimitBurst = 3;
          };
        };
      };
    in
    {
      prometheus-nginx-exporter = {
        serviceConfig.RestartSec = "10s";
        unitConfig.StartLimitIntervalSec = "60s";
      };
    }
    // builtins.listToAttrs (map makeConfig hosts);
}
