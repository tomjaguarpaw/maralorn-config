{ pkgs, config, ... }:
let
  adminCreds = pkgs.privateValue {
    adminpass = "";
    dbpass = "";
    adminuser = "";
  } "nextcloud-admin";
  inherit (config.m-0) hosts;
  certPath = "/var/lib/acme";
  nextcloudServices = hostname: {
    nextcloud-pg-backup = {
      script = let name = "nextcloud-psql-${hostname}";
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
      requires = [ "postgresql.service" "redis.service" ];
      after = [ "postgresql.service" "redis.service" ];
    };
  };
  nextcloudConf = hostname: {
    enable = true;
    hostName = hostname;
    package = pkgs.nextcloud26;
    enableBrokenCiphersForSSE = false;
    maxUploadSize = "10g";
    caching = {
      redis = true;
      apcu = false;
      memcached = false;
    };
    config = {
      dbtype = "pgsql";
      defaultPhoneRegion = "DE";
      adminuser = "maralorn";
      adminpassFile =
        builtins.toFile "nextcloud-adminpass" adminCreds.adminpass;
    };
    autoUpdateApps = {
      enable = true;
      startAt = "20:30";
    };
  };
  nextcloud-container = { v6, v4, hostname, }: {
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
    config = _: {
      imports = [ ../../roles ];
      nixpkgs = { inherit pkgs; };

      systemd.network.networks."10-wan" = {
        matchConfig.Name = "eth0";
        address = [ "${v6}/112" "${v4}/24" ];
        routes = [
          { routeConfig.Gateway = hosts.hera-intern; }
          { routeConfig.Gateway = hosts.hera-intern-v4; }
        ];
      };

      # We only login via nixos-container root-login, so we can override the
      # warning that we have no ssh or login password configured.
      users.allowNoPasswordLogin = true;

      networking = {
        useHostResolvConf = false;
        firewall.allowedTCPPorts = [ 80 443 9100 9113 ];
      };

      systemd.services = nextcloudServices hostname;
      services = {
        nextcloud = nextcloudConf hostname;
        nginx.appendHttpConfig = "access_log off;";
        redis.servers."".enable = true;

        postgresql = {
          enable = true;
          package = pkgs.postgresql_14;
          ensureDatabases = [ "nextcloud" ];
        };
      };
      system.stateVersion = "21.05";
    };
  };
  mainHostName = "cloud.maralorn.de";
in {
  systemd.services = nextcloudServices mainHostName;
  services = {
    redis.servers."".enable = true;
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
          more_set_headers "Content-Security-Policy: frame-ancestors 'self' https://mathechor.de";
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
