{ pkgs, config, ... }:
let
  inherit (config.m-0) virtualHosts;
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
    grafana =
      let
        dashboards = ./dashboards;
      in
      {
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
