flake-inputs: {config, ...}: let
  domain = "headscale.maralorn.de";
  zone = "maralorn.de";
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
          restricted_nameservers.${zone} = [config.m-0.hosts.tailscale.hera.AAAA];
          nameservers = ["1.1.1.1"];
          domains = [zone];
        };
        logtail.enabled = false;
        metrics_listen_addr = "[::1]:9098";
        ip_prefixes = config.m-0.headscaleIPs;
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
