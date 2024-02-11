{ config, lib, ... }:
let
  inherit (config.m-0) hosts;
  domain = "headscale.maralorn.de";
  zone = "maralorn.de";
  derp_port = 3479;
in
{
  networking.firewall.allowedUDPPorts = [ derp_port ];
  services = {
    headscale = {
      enable = true;
      address = "[::1]";
      port = 8289;
      settings = {
        server_url = "https://${domain}";
        derp = {
          server = {
            enabled = true;
            stun_listen_addr = "0.0.0.0:${toString derp_port}";
            region_id = "hera";
            region_code = "hera";
            region_name = "Hera";
          };
          urls = [ ];
        };
        dns_config = {
          base_domain = "m-0.eu";
          nameservers = [
            config.m-0.hosts.tailscale.hera.AAAA
            "9.9.9.9"
          ];
          domains = [ zone ];
          extra_records = lib.concatLists (
            lib.concatLists (
              lib.mapAttrsToList (
                host: ips:
                (map (
                  alias:
                  lib.mapAttrsToList (type: value: {
                    name = "${alias}.${zone}";
                    inherit type value;
                  }) (lib.filterAttrs (_: addr: addr != "") ips)
                ) (hosts.aliases.${host} or [ ]))
              ) hosts.tailscale
            )
          );
        };
        logtail.enabled = false;
        metrics_listen_addr = "[${config.m-0.hosts.tailscale.hera.AAAA}]:9098";
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

  environment.systemPackages = [ config.services.headscale.package ];
}
