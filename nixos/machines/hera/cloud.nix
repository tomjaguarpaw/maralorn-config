{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  adminCreds =
    pkgs.privateValue
    {
      adminpass = "";
      dbpass = "";
      adminuser = "";
    } "nextcloud-admin";
  inherit (config.m-0) hosts;
  certPath = "/var/lib/acme";
  nextcloudServices = hostname: {
    nextcloud-pg-backup = {
      script = let
        name = "nextcloud-psql-${hostname}";
      in ''
        ${config.services.postgresql.package}/bin/pg_dump nextcloud > /var/lib/db-backup-dumps/${name}
      '';
      serviceConfig = {
        User = "nextcloud";
        Type = "oneshot";
      };
    };
    prometheus-nginx-exporter.serviceConfig.RestartSec = 10;
    nextcloud-setup = {
      requires = ["postgresql.service" "redis.service"];
      after = ["postgresql.service" "redis.service"];
    };
  };
  nextcloudConf = hostname: {
    enable = true;
    hostName = hostname;
    package = pkgs.nextcloud25;
    enableBrokenCiphersForSSE = false;
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
      adminuser = "maralorn";
      dbpassFile = builtins.toFile "nextcloud-dbpass" adminCreds.dbpass;
      adminpassFile = builtins.toFile "nextcloud-adminpass" adminCreds.adminpass;
    };
    autoUpdateApps = {
      enable = true;
      startAt = "20:30";
    };
  };
  nextcloud-container = {
    v6,
    v4,
    hostname,
  }: {
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
    config = {pkgs, ...}: {
      imports = [../../roles];

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
        nameservers = ["213.136.95.10" "2a02:c207::1:53" "2a02:c207::2:53"];
        defaultGateway6 = {
          address = hosts.hera-intern;
          interface = "eth0";
        };
        defaultGateway = {
          address = hosts.hera-intern-v4;
          interface = "eth0";
        };
        firewall.allowedTCPPorts = [80 443 9100 9113];
      };

      systemd.services = nextcloudServices hostname;
      services = {
        nextcloud = nextcloudConf hostname;
        nginx.appendHttpConfig = "access_log off;";
        redis.servers."".enable = true;

        postgresql = {
          enable = true;
          package = pkgs.postgresql_14;
          ensureDatabases = ["nextcloud"];
        };
      };
      system.stateVersion = "21.05";
    };
  };
  mainHostName = "cloud.maralorn.de";
in {
  systemd = {
    services =
      {
        rss-server = {
          serviceConfig.ExecStart = "${pkgs.python3}/bin/python -m http.server --bind ${hosts.vpn.hera} 8842 -d /var/www/rss";
          wantedBy = ["multi-user.target"];
        };
        mastodon-digest = {
          script = ''
            ln -fs ${pkgs.privatePath "mastodon-env"} .env
            now=$(date "+%Y-%m-%d")
            mkdir -p /var/www/rss/mastodon/$now-highlights
            mkdir -p /var/www/rss/mastodon/$now-all
            ${pkgs.mastodon_digest}/bin/mastodon_digest -o /var/www/rss/mastodon/$now-highlights -n 24 -t lax --theme light
            ${pkgs.mastodon_digest}/bin/mastodon_digest -o /var/www/rss/mastodon/$now-all -n 24 -t all --theme light -f list:3811
          '';
          startAt = "19:59";
        };
      }
      // nextcloudServices mainHostName;
  };
  services = {
    redis.servers."".enable = true;
    nextcloud = nextcloudConf mainHostName;
    postgresql.ensureDatabases = ["nextcloud"];
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
            proxyPass = "http://[${hosts.chor-cloud}]";
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
  users.users.nextcloud.extraGroups = ["nginx"];
}
