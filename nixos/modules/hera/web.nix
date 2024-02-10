{ pkgs, config, ... }:
let
  locations."/".extraConfig = "return 301 https://blog.maralorn.de$request_uri;";
in
{
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
  security.acme.certs = {
    "hera.m-0.eu".keyType = "rsa4096";
  };
  services = {
    nginx = {
      enable = true;
      virtualHosts = {
        "hera.m-0.eu" = {
          default = true;
          forceSSL = true;
          enableACME = true;
          inherit locations;
        };
        "maralorn.de" = {
          enableACME = true;
          forceSSL = true;
          locations = locations // {
            "/thesis.pdf" = {
              proxyPass = "http://athene.vpn.m-0.eu/thesis.pdf";
              basicAuthFile = "/run/credentials/nginx.service/melon-pw";
            };
          };
        };
      } // pkgs.privateValue { } "extra-sites";
    };
  };
  systemd.services.nginx.serviceConfig = {
    Restart = "always";
    LoadCredential = [ "melon-pw:${config.age.secrets."basic-auth/melon".path}" ];
  };
}
