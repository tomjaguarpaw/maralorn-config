{ pkgs, config, lib, ... }:
with lib;
let
  adminCreds = pkgs.privateValue
    {
      adminpass = "";
      dbpass = "";
      adminuser = "";
    } "nextcloud-admin";
  inherit (config.m-0) hosts;
  certPath = "/var/lib/acme";
  nextcloudServices = hostname: {
    nextcloud-pg-backup = {
      script =
        let
          name = "nextcloud-psql-${hostname}";
        in
        ''
          ${config.services.postgresql.package}/bin/pg_dump nextcloud > /var/lib/db-backup-dumps/${name}
        '';
      serviceConfig = {
        User = "nextcloud";
        Type = "oneshot";
      };
    };
    prometheus-nginx-exporter.serviceConfig.RestartSec = 10;
    nextcloud-setup = {
      requires = [ "postgresql.service" "redis.service" ];
      after = [ "postgresql.service" "redis.service" ];
    };
  };
  nextcloudConf = hostname:
    {
      enable = true;
      hostName = hostname;
      package = pkgs.nextcloud22;
      maxUploadSize = "10g";
      caching = {
        redis = true;
        apcu = false;
        memcached = false;
      };
      config = {
        dbtype = "pgsql";
        dbname = "nextcloud";
        dbuser = "nextcloud";
        dbhost = "localhost";
        defaultPhoneRegion = "DE";
      } // adminCreds;
      autoUpdateApps = {
        enable = true;
        startAt = "20:30";
      };
    };
  nextcloud-container = { v6, v4, hostname }: {
    bindMounts = {
      "${certPath}" = {
        hostPath = certPath;
        isReadOnly = false;
      };
      "/var/lib/db-backup-dumps" = {
        hostPath = "/var/lib/db-backup-dumps";
        isReadOnly = false;
      };
    };
    timeoutStartSec = "360";
    autoStart = true;
    privateNetwork = true;
    hostBridge = "bridge";
    config = { pkgs, ... }: {
      imports = [ ../../roles ];

      networking = {
        interfaces.eth0 = {
          ipv6.addresses = [
            {
              address = v6;
              prefixLength = 112;
            }
          ];
          ipv4.addresses = [
            {
              address = v4;
              prefixLength = 24;
            }
          ];
        };
        inherit (config.networking) nameservers;
        defaultGateway6 = {
          address = hosts.hera-intern;
          interface = "eth0";
        };
        defaultGateway = {
          address = hosts.hera-intern-v4;
          interface = "eth0";
        };
        firewall.allowedTCPPorts = [ 80 443 ];
      };

      systemd.services = nextcloudServices hostname;
      services = {
        nextcloud = nextcloudConf hostname;
        prometheus.exporters = {
          node.openFirewall = true;
          nginx.openFirewall = true;
        };
        nginx.appendHttpConfig = "access_log off;";
        redis.enable = true;

        postgresql = {
          enable = true;
          package = pkgs.postgresql_12;
          ensureDatabases = [ "nextcloud" ];
        };
      };
    };
  };
  mainHostName = "cloud.maralorn.de";
in
{
  systemd = {
    services = {
      "container@chor-cloud" = {
        #serviceConfig.RestartSec = 10;
        unitConfig = {
          #StartLimitIntervalSec = 30;
          #StartLimitBurst = 2;
        };
      };
      rss-server = {
        serviceConfig = {
          WorkingDirectory = "/var/www/rss";
          ExecStart = "${pkgs.python3}/bin/python -m http.server 8842";
        };
        wantedBy = [ "multi-user.target" ];
      };
    } // nextcloudServices mainHostName;
  };
  services = {
    nextcloud = nextcloudConf mainHostName;
    postgresql.ensureDatabases = [ "nextcloud" ];
    nginx = {
      enable = true;
      virtualHosts."cloud.maralorn.de" = {
        enableACME = true;
        forceSSL = true;
      };
      virtualHosts."cloud.mathechor.de" = {
        enableACME = true;
        forceSSL = true;
        locations = {
          "/" = {
            proxyPass = "http://chor-cloud";
            extraConfig = "proxy_set_header Host $host; proxy_buffering off;";
          };
          "^~ /.well-known" = {
            priority = 210;
            extraConfig = ''
              location = /.well-known/carddav {
                return 301 https://$host/remote.php/dav;
              }
              location = /.well-known/caldav {
                return 301 https://$host/remote.php/dav;
              }
              location ^~ /.well-known {
                return 301 https://$host/index.php$request_uri;
              }
              try_files $uri $uri/ =404;
            '';
          };
        };
        extraConfig = ''
          more_set_headers "Content-Security-Policy: frame-ancestors 'self' https://*.mathechor.de";
        '';
      };
    };
  };
  m-0.monitoring = [
    {
      name = "chor-cloud container";
      host = "chor-cloud:9100";
      container = true;
    }
    {
      name = "chor-cloud nginx";
      host = "chor-cloud:9113";
      container = true;
    }
  ];
  containers = {
    chor-cloud = nextcloud-container {
      hostname = "cloud.mathechor.de";
      v6 = hosts.chor-cloud;
      v4 = hosts.chor-cloud-intern-v4;
    };
  };
  users.users.nextcloud.extraGroups = [ "nginx" ];
}
