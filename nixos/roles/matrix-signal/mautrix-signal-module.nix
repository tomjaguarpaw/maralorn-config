{ config, pkgs, lib, ... }:

with lib;

let
  dataDir = "/var/lib/mautrix-signal";
  registrationFile = "${dataDir}/signal-registration.yaml";
  cfg = config.services.mautrix-signal;
  settingsFormat = pkgs.formats.json { };
  settingsFileUnsubstituted = settingsFormat.generate "mautrix-signal-config-unsubstituted.json" cfg.settings;
  settingsFile = "${dataDir}/config.json";

in
{
  options = {
    services.mautrix-signal = {
      enable = mkEnableOption "Mautrix-Signal, a Matrix-Signal hybrid puppeting bridge";

      settings = mkOption rec {
        apply = recursiveUpdate default;
        inherit (settingsFormat) type;
        default = {
          appservice = rec {
            database = "postgresql:///mautrix-signal?host=/run/postgresql";
            database_opts = { };
            hostname = "0.0.0.0";
            port = 29328;
            address = "http://localhost:${toString port}";
          };

          bridge = {
            permissions = { };
            double_puppet_server_map = { };
            login_shared_secret_map = { };
          };

          logging = {
            version = 1;

            formatters.precise.format = "[%(levelname)s@%(name)s] %(message)s";

            handlers.console = {
              class = "logging.StreamHandler";
              formatter = "precise";
            };

            loggers = {
              mau.level = "INFO";
              telethon.level = "INFO";

              # prevent tokens from leaking in the logs:
              # https://github.com/tulir/mautrix-signal/issues/351
              aiohttp.level = "WARNING";
            };

            # log to console/systemd instead of file
            root = {
              level = "INFO";
              handlers = [ "console" ];
            };
          };
        };
        example = literalExample ''
          {
            homeserver = {
              address = "http://localhost:8008";
              domain = "public-domain.tld";
            };

            appservice.public = {
              prefix = "/public";
              external = "https://public-appservice-address/public";
            };

            bridge.permissions = {
              "example.com" = "full";
              "@admin:example.com" = "admin";
            };
          }
        '';
        description = ''
          <filename>config.yaml</filename> configuration as a Nix attribute set.
          Configuration options should match those described in
          <link xlink:href="https://github.com/tulir/mautrix-signal/blob/master/mautrix_signal/example-config.yaml">
          example-config.yaml</link>.
        '';
      };

      serviceDependencies = mkOption {
        type = with types; listOf str;
        default = optional config.services.matrix-synapse.enable "matrix-synapse.service";
        description = ''
          List of Systemd services to require and wait for when starting the application service.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.postgresql =
      {
        ensureDatabases = [ "mautrix-signal" ];
        ensureUsers = [
          {
            name = "mautrix-signal";
            ensurePermissions = {
              "DATABASE \"mautrix-signal\"" = "ALL PRIVILEGES";
            };
          }
        ];
      };
    services.matrix-synapse.app_service_config_files = [ registrationFile ];
    systemd.services.mautrix-signal = {
      description = "Mautrix-Signal, a Matrix-Signal hybrid puppeting/relaybot bridge.";

      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" "signald.target" ];
      after = [ "network-online.target" "signald.target" ];

      preStart = ''
        old_umask=$(umask)
        makeSettingsFile () {
          tempjson=$(${pkgs.coreutils}/bin/mktemp)
          ${pkgs.yq}/bin/yq . '${registrationFile}' > "$tempjson"
          [ -f ${settingsFile} ] && rm -f ${settingsFile}
          umask 0277
          ${pkgs.jq}/bin/jq '.[0] * { appservice : { as_token: .[1].as_token, hs_token: .[1].hs_token }}' \
            -s '${settingsFileUnsubstituted}' $tempjson > '${settingsFile}'
          rm $tempjson
          umask $old_umask
        }

        if [ -f '${registrationFile}' ]; then
          makeSettingsFile
        else
          umask 0277
          cp '${settingsFileUnsubstituted}' '${settingsFile}'
          umask 0077
          # generate the appservice's registration file if absent
          ${pkgs.mautrix-signal}/bin/mautrix-signal \
            --generate-registration \
            --config='${settingsFile}' \
            --registration='${registrationFile}' \
            -n
          umask $old_umask
          makeSettingsFile
        fi
        # Allow synapse access to the registration
        if ${getBin pkgs.glibc}/bin/getent group matrix-synapse > /dev/null; then
          chgrp matrix-synapse ${registrationFile}
          chmod g+r ${registrationFile}
        fi
      '';

      serviceConfig = rec {
        Type = "simple";
        Restart = "always";

        ProtectSystem = "full";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        User = "mautrix-signal";
        Group = "signald";

        CapabilityBoundingSet = [ "CAP_CHOWN" ];
        AmbientCapabilities = CapabilityBoundingSet;
        NoNewPrivileges = true;

        LockPersonality = true;
        RestrictRealtime = true;

        SupplementaryGroups = [ "signald" ];
        BindPaths = "/var/lib/signald";
        StateDirectory = baseNameOf dataDir;
        StateDirectoryMode = "771";
        UMask = 0007;

        ExecStart = ''
          ${pkgs.mautrix-signal}/bin/mautrix-signal \
            --config='${settingsFile}'
        '';
      };
      unitConfig = {
        JoinsNamespaceOf = "signald.service";
      };

      restartTriggers = [ settingsFileUnsubstituted ];
    };
    users.users.mautrix-signal = {
      description = "Service user for the Matrix-Signal bridge";
      group = "signald";
      isSystemUser = true;
    };
  };

  meta.maintainers = with maintainers; [ expipiplus1 ];
}
