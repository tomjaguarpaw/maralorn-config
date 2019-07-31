{ config, ... }:
let
  hostName = "matrix.maralorn.de";
  inherit (config.m-0) hosts;
  inherit (../../common/lib.nix) unstable;
in {
  networking.firewall.allowedTCPPorts = [ 3478 8448 ];

  m-0.monitoring = [
    {
      name = "matrix";
      host = "matrix:9100";
    }
    {
      name = "matrix-nginx";
      host = "matrix:9113";
    }
  ];

  services.coturn = {
    enable = true;
    pkey = "/var/lib/acme/hera.m-0.eu/key.pem";
    cert = "/var/lib/acme/hera.m-0.eu/fullchain.pem";
    no-tcp = true;
    static-auth-secret = config.m-0.private.turn_secret;
    realm = "maralorn.de";
    use-auth-secret = true;
  };

  containers.matrix = {
    autoStart = true;
    privateNetwork = true;
    hostBridge = "bridge";
    config = { pkgs, lib, ... }: {
      imports = [ ../../system ];
      networking = {
        interfaces.eth0 = {
          ipv6.addresses = [{
            address = hosts.matrix;
            prefixLength = 112;
          }];
          ipv4.addresses = [{
            address = hosts.matrix-intern-v4;
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
        firewall.allowedTCPPorts = [ 80 443 8448 ];
      };
      m-0.riot = {
        enable = true;
        hostname = "riot.maralorn.de";
        config = {
          default_hs_url = "https://matrix.maralorn.de";
          default_is_url = "https://vector.im";
          integrations_ui_url = "";
          integrations_rest_url = "";
          integrations_widgets_urls = [ ];
          bug_report_endpoint_url = "https://riot.im/bugreports/submit";
          welcomeUserId = "@riot-bot:matrix.org";
          piwik = false;
          features = {
            feature_lazyloading = "enable";
            feature_room_breadcrumbs = "enable";
          };
          roomDirectory = { servers = [ "matrix.org" "maralorn.de" ]; };
          branding = {
            welcomeBackgroundUrl =
              "https://cloud.maralorn.de/apps/theming/image/background";
          };
        };
      };
      services = {
        nginx = {
          enable = true;
          virtualHosts."${hostName}" = {
            forceSSL = true;
            enableACME = true;
            locations = {
              "/" = {
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
              resources = [
                {
                  compress = true;
                  names = [ "client" ];
                }
                {
                  compress = false;
                  names = [ "federation" ];
                }
              ];
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
    };
  };

}
