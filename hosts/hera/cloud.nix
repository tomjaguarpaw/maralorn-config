{ config, lib, ... }:
with lib;
let
  inherit (config.m-0.private) me cloud;
  inherit (config.m-0) hosts;
  nextcloud-container = { v6, v4, hostname , news-updater ? false}: {
    autoStart = true;
    privateNetwork = true;
    hostBridge = "bridge";
    config = { pkgs, ... }: {
      disabledModules = [ "services/web-apps/nextcloud.nix" ];
      imports = [
        ../../system
        ./nextcloud.nix
      ];

      networking = {
        interfaces.eth0 = {
          ipv6.addresses = [{ address = v6; prefixLength = 112; }];
          ipv4.addresses = [{ address = v4; prefixLength = 24; }];
        };
        inherit (config.networking) nameservers;
        defaultGateway6 = { address = hosts.hera-intern; interface = "eth0"; };
        defaultGateway = { address = hosts.hera-intern-v4; interface = "eth0"; };
        firewall.allowedTCPPorts = [ 80 443 ];
      };

      services = {

        nginx = {
          virtualHosts."${hostname}" = {
            forceSSL = true;
            enableACME = true;
            default = true;
          };
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

        redis = {
          enable = true;
        };

        postgresql = {
          enable = true;
          initialScript = pkgs.writeText "psql-init" ''
            create role nextcloud with login password '${cloud.dbpass}';
            create database nextcloud with owner nextcloud;
          '';
        };
      };
      systemd = {
        services ={
          "nextcloud-setup"= {
            requires = ["postgresql.service"];
            after = [
              "postgresql.service"
            ];
          };
          "nextcloud-news-updater" = mkIf news-updater {
            startAt = "20:00";
            serviceConfig = {
              Type = "oneshot";
            };
            script = let
              config = pkgs.writeText "updater.ini" (generators.toINI {} {
                updater = {
                  user = cloud.adminuser;
                  password = cloud.adminpass;
                  url = "https://${hostname}/";
                  mode = "singlerun";
                };});
            in
              "${pkgs.nextcloud-news-updater}/bin/nextcloud-news-updater -c ${config}";
          };
        };
      };
    };
  };

in {
  m-0.monitoring = [
    { name = "mathechor-cloud"; host = "mathechor-cloud:9100"; }
    { name = "mathechor-cloud-nginx"; host = "mathechor-cloud:9113"; }
    { name = "cloud"; host = "cloud:9100"; }
    { name = "cloud-nginx"; host = "cloud:9113"; }
  ];
  containers = {
    chor-cloud = nextcloud-container {
      hostname = "cloud.mathechor.de";
      v6 = hosts.mathechor-cloud;
      v4 = hosts.mathechor-cloud-intern-v4;
    };
    cloud = nextcloud-container {
      hostname = "cloud.maralorn.de";
      v6 = hosts.cloud;
      v4 = hosts.cloud-intern-v4;
      news-updater = true;
    };
  };
}
