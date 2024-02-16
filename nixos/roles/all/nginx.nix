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
}
