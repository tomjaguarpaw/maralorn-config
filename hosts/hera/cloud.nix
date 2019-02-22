{ config, ... }:
let
  inherit (config.m-0.private) me cloud;
in {
  containers.cloud = {
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
          ipv6.addresses = [{ address = config.m-0.hosts.cloud; prefixLength = 112; }];
        };
        inherit (config.networking) nameservers;
        defaultGateway6 = { address = config.m-0.hosts.hera-intern; interface = "eth0"; };
        firewall.allowedTCPPorts = [ 80 443 ];
      };

      services = {

        nginx = {
          virtualHosts."cloud.maralorn.de" = {
            forceSSL = true;
            enableACME = true;
          };
        };

        nextcloud = {
          enable = true;
          hostName = "cloud.maralorn.de";
          nginx.enable = true;
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
      systemd.services."nextcloud-setup"= {
        requires = ["postgresql.service"];
        after = [
          "postgresql.service"
        ];
      };
    };
  };
}
