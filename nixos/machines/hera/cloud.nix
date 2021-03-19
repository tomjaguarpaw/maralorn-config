{ pkgs, config, lib, ... }:
with lib;
let
  adminCreds = pkgs.privateValue {
    adminpass = "";
    dbpass = "";
    adminuser = "";
  } "nextcloud-admin";
  inherit (config.m-0) hosts;
  certPath = "/var/lib/acme";
  nextcloud-container = { v6, v4, hostname, rss ? false, extraMounts ? {} }: {
    bindMounts = {
      "${certPath}" = {
        hostPath = certPath;
        isReadOnly = false;
      };
      "/var/lib/db-backup-dumps" = {
        hostPath = "/var/lib/db-backup-dumps";
        isReadOnly = false;
      };
    } // extraMounts;
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

      services = {
        prometheus.exporters = {
          node.openFirewall = true;
          nginx.openFirewall = true;
        };

        nextcloud = {
          enable = true;
          hostName = hostname;
          package = pkgs.nextcloud21;
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

        redis.enable = true;

        postgresql = {
          enable = true;
          package = pkgs.postgresql_12;
        };
      };
      systemd = {
        services = {
          rss-server = mkIf rss {
            serviceConfig = {
              WorkingDirectory = "/var/www/rss";
              ExecStart = "${pkgs.python3}/bin/python -m http.server 8842";
            };
            wantedBy = [ "multi-user.target" ];
          };
          pg_backup = {
            script = let
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
            requires = [ "postgresql.service" ];
            after = [ "postgresql.service" ];
          };
          nextcloud-news-updater = mkIf rss {
            startAt = "20:00";
            serviceConfig = {
              Type = "oneshot";
              User = "nextcloud";
              ExecStart = let
                config = pkgs.writeText "updater.ini" (
                  generators.toINI {} {
                    updater = {
                      user = adminCreds.adminuser;
                      password = adminCreds.adminpass;
                      url = "https://${hostname}/";
                      mode = "singlerun";
                    };
                  }
                );
              in
                "${pkgs.nextcloud-news-updater}/bin/nextcloud-news-updater -c ${config}";
            };
          };
        };
      };
    };
  };
  serviceConfig.RestartSec = 10;
  unitConfig = {
    StartLimitIntervalSec = 30;
    StartLimitBurst = 2;
  };
  nginx = {
    rootExtra = "proxy_set_header Host $host; proxy_buffering off;";
    wellKnown = {
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
in
{
  systemd.services."container@cloud" = { inherit serviceConfig unitConfig; };
  systemd.services."container@chor-cloud" = {
    inherit serviceConfig unitConfig;
  };
  services = {
    nginx = {
      enable = true;
      virtualHosts."cloud.maralorn.de" = {
        enableACME = true;
        forceSSL = true;
        locations = {
          "/" = {
            proxyPass = "http://cloud";
            extraConfig = nginx.rootExtra;
          };
          "^~ /.well-known" = nginx.wellKnown;
        };
      };
      virtualHosts."cloud.mathechor.de" = {
        enableACME = true;
        forceSSL = true;
        locations = {
          "/" = {
            proxyPass = "http://chor-cloud";
            extraConfig = nginx.rootExtra;
          };
          "^~ /.well-known" = nginx.wellKnown;
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
    {
      name = "cloud container";
      host = "cloud:9100";
      container = true;
    }
    {
      name = "cloud nginx";
      host = "cloud:9113";
      container = true;
    }
  ];
  containers = {
    chor-cloud = nextcloud-container {
      hostname = "cloud.mathechor.de";
      v6 = hosts.chor-cloud;
      v4 = hosts.chor-cloud-intern-v4;
    };
    cloud = nextcloud-container {
      hostname = "cloud.maralorn.de";
      v6 = hosts.cloud;
      v4 = hosts.cloud-intern-v4;
      rss = true;
      extraMounts = {
        "/media" = {
          hostPath = "/media";
          isReadOnly = false;
        };
        "/var/www/rss" = { hostPath = "/var/www/rss"; };
      };
    };
  };
}
