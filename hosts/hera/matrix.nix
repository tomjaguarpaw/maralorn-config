{ config, ... }:
let
  hostName = "matrix.maralorn.de";
  inherit (config.m-0) hosts;
  inherit (../../lib) unstable;
in {
  networking.firewall.allowedTCPPorts = [ 3478 8448 ];

  services = {
    coturn = {
      enable = true;
      pkey = "/var/lib/acme/hera.m-0.eu/key.pem";
      cert = "/var/lib/acme/hera.m-0.eu/fullchain.pem";
      no-tcp = true;
      static-auth-secret = config.m-0.private.turn_secret;
      realm = "maralorn.de";
      use-auth-secret = true;
    };
    nginx = {
      enable = true;
      virtualHosts."${hostName}" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            proxyPass = "http://[::1]:8008";
            extraConfig = ''
              proxy_set_header X-Forwarded-For $remote_addr;
            '';
          };
        };
      };
    };

    # Postgres
    postgresql = { enable = true; };

    # Synapse
    matrix-synapse = {
      enable = true;
      enable_metrics = true;
      server_name = "maralorn.de";
      public_baseurl = "https://${hostName}";
      url_preview_enabled = true;
      database_type = "psycopg2";
      max_upload_size = "30M";
      create_local_database = false;
      dynamic_thumbnails = true;
      macaroon_secret_key = config.m-0.private.macaroon_secret;
      turn_uris = [ "turn:hera.m-0.eu:3478?transport=udp" ];
      turn_shared_secret = config.m-0.private.turn_secret;
      turn_user_lifetime = "5h";
      allow_guest_access = true;
      logConfig = ''
        version: 1

        formatters:
          journal_fmt:
            format: '%(name)s: [%(request)s] %(message)s'

        filters:
          context:
            (): synapse.util.logcontext.LoggingContextFilter
            request: ""

        handlers:
          journal:
            class: systemd.journal.JournalHandler
            formatter: journal_fmt
            filters: [context]
            SYSLOG_IDENTIFIER: synapse

        disable_existing_loggers: True

        loggers:
          synapse:
            level: WARN
          synapse.storage.SQL:
            level: WARN

        root:
          level: WARN
          handlers: [journal]
      '';
      database_args = {
        user = "matrix-synapse";
        database = "matrix-synapse";
        cp_min = 5;
        cp_max = 10;
      };
      report_stats = true;
      tls_certificate_path = "/var/lib/acme/${hostName}/fullchain.pem";
      tls_private_key_path = "/var/lib/acme/${hostName}/key.pem";
      listeners = [
        {
          port = 8448;
          bind_address = "::";
          resources = [{
            compress = false;
            names = [ "federation" ];
          }];
          x_forwarded = false;
        }
        {
          port = 8008;
          bind_address = "::1";
          resources = [
            {
              compress = false;
              names = [ "client" ];
            }
            {
              compress = false;
              names = [ "federation" ];
            }
          ];
          x_forwarded = true;
          tls = false;
        }
      ];
    };
  };
  security.acme.certs = {
    "${hostName}" = {
      group = "matrix-synapse";
      allowKeysForGroup = true;
      postRun =
        "systemctl reload nginx.service; systemctl restart matrix-synapse.service";
    };
  };

}
