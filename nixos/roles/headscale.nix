flake-inputs: {config, ...}: let
  domain = "headscale.maralorn.de";
in {
  disabledModules = [
    "services/networking/headscale.nix"
  ];
  imports = [
    "${flake-inputs.nixos-unstable}/nixos/modules/services/networking/headscale.nix"
  ];
  m-0.monitoring = [
    {
      name = "hera-headscale";
      host = "[::1]:9098";
    }
  ];
  services = {
    headscale = {
      enable = true;
      address = "[::1]";
      port = 8289;
      settings = {
        server_url = "https://${domain}";
        dns_config = {
          base_domain = "m-0.eu";
          restricted_nameservers."maralorn.de" = ["100.64.7.1"];
          nameservers = ["1.1.1.1" "9.9.9.9"];
          domains = ["maralorn.de"];
        };
        logtail.enabled = false;
        metrics_listen_addr = "[::1]:9098";
        ip_prefixes = [
          "100.64.7.0/24"
          "fd7a:115c:a1e0:77::/64"
        ];
      };
    };

    nginx.virtualHosts.${domain} = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.headscale.port}";
        proxyWebsockets = true;
      };
    };
  };

  environment.systemPackages = [config.services.headscale.package];
}
