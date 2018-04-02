{ config, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [ 8448 ];

  users.users = {
    matrix-synapse.extraGroups = ["matrix-cert"];
  };

  users.groups = {
    matrix-cert = {};
  };

  security.acme.certs = {
    "matrix.maralorn.de" = {
      email = "malte.brandy@maralorn.de";
      postRun = "systemctl restart matrix-synapse";
      allowKeysForGroup = true;
      group = "matrix-cert";
      extraDomains = {
        "maralorn.de" = null;
      };
    };
  };
  services = {
    nginx = {
      enable = true;
      virtualHosts."matrix.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/_matrix" = {
            proxyPass = "http://[::1]:8008";
            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header X-Forwarded-For $remote_addr;
            '';
          };
        };
      };
    };

    # Postgres
    postgresql = {
      enable = true;
      package = pkgs.postgresql96;
    };

    # Synapse
    matrix-synapse = {
      enable = true;
      package = pkgs.matrix-synapse;
      server_name = "maralorn.de";
      database_type = "psycopg2";
      max_upload_size = "30M";
      database_args = {
        user = "matrix-synapse";
        database = "matrix-synapse";
        cp_min = "5";
        cp_max = "10";
      };
      report_stats = true;
      tls_certificate_path = "/var/lib/acme/matrix.maralorn.de/fullchain.pem";
      tls_private_key_path = "/var/lib/acme/matrix.maralorn.de/key.pem";
      listeners = [
        {
          port = 8448;
          bind_address = "::";
          resources = [ { compress = true; names = [ "client" ]; } { compress = false; names = [ "federation" ]; } ];
          x_forwarded = false;
        }
        {
          port = 8008;
          bind_address = "::1";
          resources = [ { compress = false; names = [ "client" ]; } { compress = false; names = [ "federation" ]; } ];
          x_forwarded = true;
          tls = false;
        }
      ];
    };
  };
}
