{ config, lib, ... }:
with lib;
let
  inherit (config.m-0.private) me cloud;
  inherit (config.m-0) hosts;
  certPath = "/var/lib/acme";
  nextcloud-container = { v6, v4, hostname, news-updater ? false }: {
    bindMounts = {
      "${certPath}" = {
        hostPath = certPath;
        isReadOnly = false;
      };
    };
    autoStart = true;
    privateNetwork = true;
    hostBridge = "bridge";
    config = { pkgs, ... }: {
      disabledModules = [ "services/web-apps/nextcloud.nix" ];
      imports = [ ../../system ./nextcloud.nix ];

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
            #extraTrustedDomains = [ "2a02:c207:3002:7584::3:1" ];
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

in {
  systemd.services."container@".serviceConfig = {
    RestartSec = 10;
    TimeoutSec = 360;
  };
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
    };
  };
}
