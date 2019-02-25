{ config, ... }:
let
  inherit (config.m-0) hosts;
  certPath = "/var/lib/acme/hera.m-0.eu";
in
{
networking.firewall.allowedTCPPorts = [ 80 443 ];
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
    table {
      cloud.maralorn.de ${hosts.cloud}
      cloud.mathechor.de ${hosts.mathechor-cloud}
      .* ${hosts.web}
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
        };
      };
    };
  };
};

}
