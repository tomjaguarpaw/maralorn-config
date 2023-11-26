{
  pkgs,
  config,
  lib,
  ...
}:
let
  server_name = "maralorn.de";
  hostName = "matrix.${server_name}";
in
{
  environment.systemPackages = [ pkgs.matrix-synapse-tools.rust-synapse-compress-state ];
  systemd.services = {
    # use jemalloc to improve the memory situation with synapse
    matrix-synapse.environment = {
      LD_PRELOAD = "${pkgs.jemalloc}/lib/libjemalloc.so";
      SYNAPSE_CACHE_FACTOR = "1.0";
      LimitNOFILE = "4096";
    };
    synapse-cleanup = {
      serviceConfig = {
        ExecStart =
          pkgs.writeHaskell "synapse-cleanup"
            {
              libraries = builtins.attrValues pkgs.myHaskellScriptPackages ++ [
                pkgs.haskellPackages.postgresql-simple
                pkgs.haskellPackages.HTTP
              ];
              ghcEnv.PATH = "${
                lib.makeBinPath [
                  pkgs.matrix-synapse-tools.rust-synapse-compress-state
                  config.services.postgresql.package
                ]
              }:$PATH";
              ghcArgs = [ "-threaded" ];
            }
            (builtins.readFile ./synapse-cleanup.hs);
        User = "matrix-synapse";
        Type = "oneshot";
      };
    };
    synapse-worker-1 = { };
  };
  services = {
    nginx = {
      enable = true;
      virtualHosts."${server_name}" = {
        enableACME = true;
        forceSSL = true;
        locations = {
          "/.well-known/matrix/server".extraConfig =
            let
              server."m.server" = "${hostName}:443";
            in
            ''
              add_header Content-Type application/json;
              return 200 '${builtins.toJSON server}';
            '';
          "/.well-known/matrix/client".extraConfig =
            let
              client."m.homeserver".base_url = "https://${hostName}";
            in
            ''
              add_header Content-Type application/json;
              add_header Access-Control-Allow-Origin *;
              return 200 '${builtins.toJSON client}';
            '';
        };
      };
      virtualHosts."${hostName}" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://[::1]:8008";
          extraConfig = ''
            proxy_set_header X-Forwarded-For $remote_addr;
            proxy_set_header X-Forwarded-Proto https;'';
        };
      };
    };

    # Postgres
    postgresql = {
      enable = true;
      settings = import ./postgres-tuning.nix;
      ensureDatabases = [ "matrix-synapse" ];
    };

    # Synapse
    matrix-synapse = {
      enable = true;
      settings =
        let
          server-secrets =
            pkgs.privateValue
              {
                registration_shared_secret = "";
                macaroon_secret_key = "";
              }
              "matrix/server-secrets";
        in
        server-secrets
        // {
          enable_metrics = true;
          serve_server_wellknown = true;
          inherit server_name;
          public_baseurl = "https://${hostName}";
          url_preview_enabled = true;
          database_type = "psycopg2";
          max_upload_size = "30M";
          dynamic_thumbnails = true;
          #turn_shared_secret = config.services.coturn.static-auth-secret;
          #turn_uris = let
          #  turns = "turns:${config.services.coturn.realm}:${
          #    toString config.services.coturn.tls-listening-port
          #  }";
          #  turn = "turn:${config.services.coturn.realm}:${
          #    toString config.services.coturn.listening-port
          #  }";
          #in [
          #  "${turns}?transport=udp"
          #  "${turns}?transport=tcp"
          #  "${turn}?transport=udp"
          #  "${turn}?transport=tcp"
          #];
          #turn_user_lifetime = "24h";
          #allow_guest_access = true;
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
          listeners = [
            {
              type = "metrics";
              port = 9148;
              bind_addresses = [ "127.0.0.1" ];
              resources = [ ];
              tls = false;
            }
            {
              port = 8008;
              bind_addresses = [ "::1" ];
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
  };
}
