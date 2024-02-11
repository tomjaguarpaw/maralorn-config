{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (config.m-0) virtualHosts;
  heading = name: link: ''<h2><a href=\"${link}\">${name}</a></h2>'';
  badge = src: link: ''<a href=\"${link}\">\n  <img src=\"${src}\">\n</a>'';
  job = name: badge "https://ci.maralorn.de/badge/${name}.svg" "https://ci.maralorn.de/jobs/${name}";
  badges = lib.concatStringsSep "\\n" [
    (heading "ci.maralorn.de" "https://ci.maralorn.de")
    (job "test-config")
    (job "blog")

    (heading "haskell-taskwarrior" "https://hackage.haskell.org/package/taskwarrior")
    (badge "https://github.com/maralorn/haskell-taskwarrior/actions/workflows/haskell.yml/badge.svg" "https://github.com/maralorn/haskell-taskwarrior/actions"
    )
    (badge "https://img.shields.io/hackage-deps/v/taskwarrior.svg" "http://packdeps.haskellers.com/reverse/taskwarrior"
    )
    (badge "https://repology.org/badge/vertical-allrepos/haskell:taskwarrior.svg?columns=3&header=" "https://repology.org/project/haskell:taskwarrior/versions"
    )

    (heading "nix-output-monitor" "https://github.com/maralorn/nix-output-monitor")
    (badge "https://repology.org/badge/vertical-allrepos/nix-output-monitor.svg?columns=3&header=" "https://repology.org/project/nix-output-monitor/versions"
    )
  ];
  dashboards = pkgs.runCommand "dashboards" { } ''
    mkdir -p $out
    cp ${./dashboards}/* $out
    substituteInPlace $out/health-status.json --replace '@BADGES@' '${badges}'
  '';
in
{
  systemd.services.setup-accounting-db = {
    script = ''
      set -ex
      pw=$(cat ${config.age.secrets."grafana-postgres-pw".path})
      ${config.services.postgresql.package}/bin/psql -d accounting << EOF
      CREATE TABLE IF NOT EXISTS balances (account text, date date, amount numeric(11,2));
      CREATE TABLE IF NOT EXISTS flat_balances (account text, date date, amount numeric(11,2));
      GRANT ALL ON balances TO maralorn;
      GRANT SELECT ON balances TO grafana;
      GRANT ALL ON flat_balances TO maralorn;
      GRANT SELECT ON flat_balances TO grafana;
      ALTER ROLE grafana PASSWORD '$pw';
      EOF
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
    };
    after = [ "postgresql.service" ];
    wantedBy = [ "default.target" ];
  };

  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
      ensureDatabases = [ "accounting" ];
      ensureUsers = [
        { name = "maralorn"; }
        { name = "grafana"; }
      ];
    };
    nginx = {
      enable = true;
      virtualHosts.${virtualHosts."graphs"} = {
        locations."/".proxyPass = "http://localhost:3000/";
      };
    };
    grafana = {
      enable = true;
      settings = {
        "auth.anonymous" = {
          org_role = "Admin";
          enabled = true;
        };
        security.allow_embedding = true;
        users.default_theme = "dark";
        "auth.basic".enabled = false;
        server.domain = virtualHosts."graphs";
        dashboards.default_home_dashboard_path = "${dashboards}/accounting.json";
      };
      provision = {
        enable = true;
        datasources.settings.datasources = [
          {
            access = "proxy";
            name = "prometheus";
            type = "prometheus";
            url = "http://localhost:9090";
          }
          {
            type = "postgres";
            isDefault = true;
            name = "Postgres";
            url = "localhost:5432";
            user = "grafana";
            uid = "accounting";
            secureJsonData.password = "$__file{${config.age.secrets."grafana-postgres-pw".path}}";
            jsonData = {
              database = "accounting";
              sslmode = "disable";
            };
          }
        ];
        dashboards.settings.providers = [
          {
            name = "Static dashboards";
            options.path = dashboards;
          }
        ];
      };
    };
  };
}
