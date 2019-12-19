{ config, lib, ... }:
with lib;
let
  inherit (config.m-0.private) me cloud;
  inherit (config.m-0) hosts;
  certPath = "/var/lib/acme";
  nextcloud-container =
    { v6, v4, hostname, news-updater ? false, extraMounts ? { } }: {
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
        imports = [ ../../system ];

        networking = {
          interfaces.eth0 = {
            ipv6.addresses = [{
              address = v6;
              prefixLength = 112;
            }];
            ipv4.addresses = [{
              address = v4;
              prefixLength = 24;
            }];
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
            nginx.enable = true;
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
              inherit (cloud) adminpass dbpass adminuser;
            };
            autoUpdateApps = {
              enable = true;
              startAt = "20:30";
            };
          };

          redis = { enable = true; };

          postgresql = {
            enable = true;
            initialScript = pkgs.writeText "psql-init" ''
              create role nextcloud with login password '${cloud.dbpass}';
              create database nextcloud with owner nextcloud;
            '';
          };
        };
        systemd = {
          services = {
            pg_backup = {
              script = let name = "nextcloud-psql-${hostname}";
              in ''
                ${pkgs.postgresql}/bin/pg_dump nextcloud > /var/lib/db-backup-dumps/tmp/${name}
                ${pkgs.coreutils}/bin/mv /var/lib/db-backup-dumps/tmp/${name} /var/lib/db-backup-dumps/cur/${name}
              '';
              serviceConfig = {
                User = "nextcloud";
                Type = "oneshot";
              };
              startAt = "23:00";
            };
            "prometheus-nginx-exporter" = {
              serviceConfig = { RestartSec = 10; };
            };
            "nextcloud-setup" = {
              requires = [ "postgresql.service" ];
              after = [ "postgresql.service" ];
            };
            "nextcloud-news-updater" = mkIf news-updater {
              startAt = "20:00";
              serviceConfig = {
                Type = "oneshot";
                User = "nextcloud";
                ExecStart = let
                  config = pkgs.writeText "updater.ini" (generators.toINI { } {
                    updater = {
                      user = cloud.adminuser;
                      password = cloud.adminpass;
                      url = "https://${hostname}/";
                      mode = "singlerun";
                    };
                  });
                in "${pkgs.nextcloud-news-updater}/bin/nextcloud-news-updater -c ${config}";
              };
            };
          };
        };
      };
    };
  serviceConfig = { RestartSec = 10; };
in {
  systemd.services."container@cloud" = { inherit serviceConfig; };
  systemd.services."container@chor-cloud" = { inherit serviceConfig; };
  services = {
    nginx = {
      enable = true;
      virtualHosts."cloud.maralorn.de" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://cloud";
          extraConfig = "proxy_set_header Host $host;";
        };
      };
      virtualHosts."cloud.mathechor.de" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://chor-cloud";
          extraConfig = "proxy_set_header Host $host;";
        };
        extraConfig = ''
          more_set_headers "Content-Security-Policy: frame-ancestors 'self' https://*.mathechor.de";
        '';
      };
    };
  };
  m-0.monitoring = [
    {
      name = "chor-cloud";
      host = "chor-cloud:9100";
    }
    {
      name = "chor-cloud-nginx";
      host = "chor-cloud:9113";
    }
    {
      name = "cloud";
      host = "cloud:9100";
    }
    {
      name = "cloud-nginx";
      host = "cloud:9113";
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
      news-updater = true;
      extraMounts = {
        "/media" = {
          hostPath = "/media";
          isReadOnly = false;
        };
      };
    };
  };
}
