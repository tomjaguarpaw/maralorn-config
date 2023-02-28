{config, ...}: let
  domain = "vpn.m-0.eu";
in {
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
      serverUrl = "https://${domain}";
      dns = {
        baseDomain = "m-0.eu";
        nameservers = ["100.64.7.1" "1.1.1.1" "9.9.9.9"];
      };
      settings = {
        logtail.enabled = false;
        metrics_listen_addr = "[::1]:9098";
        ip_prefixes = [
          "100.64.7.0/24"
          "fd07::/48"
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
