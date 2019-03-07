{ config, ... }:
let
  inherit (config.m-0) hosts;
  certPath = "/var/lib/acme/hera.m-0.eu";
in
{
networking.firewall.allowedTCPPorts = [ 80 443 ];
m-0.monitoring = [
    { name = "web"; host = "web:9100"; }
    { name = "web-nginx"; host = "web:9113"; }
];
services.sniproxy = {
  enable = true;
  config = ''
    error_log {
      syslog daemon
    }
    access_log {
      syslog daemon
    }
    listen 80 {
      proto http
    }
    listen 443 {
      proto tls
    }
    listen 8448 {
      proto tls
      table matrix

      fallback ${hosts.matrix}:8448
    }
    table {
      cloud.maralorn.de ${hosts.cloud}
      cloud.mathechor.de ${hosts.mathechor-cloud}
      matrix.maralorn.de ${hosts.matrix}
      riot.maralorn.de ${hosts.matrix}
      .* ${hosts.web}
    }
    table matrix {
      .* ${hosts.matrix}
    }
  '';
};
containers.web = {
  bindMounts = { "${certPath}" = { hostPath = certPath; }; };
  autoStart = true;
  privateNetwork = true;
  hostBridge = "bridge";
  config = { pkgs, lib, ... }: {
    imports = [../../system];
    networking = {
      interfaces.eth0 = {
        ipv6.addresses = [{ address = config.m-0.hosts.web; prefixLength = 112; }];
      };
      inherit (config.networking) nameservers;
      defaultGateway6 = { address = config.m-0.hosts.hera-intern; interface = "eth0"; };
      firewall.allowedTCPPorts = [ 80 443 ];
    };
    m-0 = {
      blog.enable = true;
      mathechor-de = {
        enable = true;
        password = config.m-0.private.mathechor-pw;
      };
    };
    services = {
      nginx = {
        enable = true;
        virtualHosts."hera.m-0.eu" = {
          enableACME = true;
          forceSSL = true;
          locations = {
            "/" = {
              extraConfig = ''
                return 200 "Hello there. I hope you are having a very nice day! If you don't know what to find here, you probably don't care about this domain.";
              '';
            };
          };
        };
        virtualHosts."maralorn.de" = {
          enableACME = true;
          forceSSL = true;
          locations = {
            "/.well-known/matrix/server" = {
              extraConfig = ''
                default_type application/json;
                return 200 "{\"m.server\": \"matrix.maralorn.de:443\"}";
              '';
            };
            "/" = {
              extraConfig = ''
                return 200 "Hello there. I hope you are having a very nice day! If you don't know what to find here, you probably don't care about this domain.";
              '';
            };
          };
        };
      };
    };
  };
};

}
