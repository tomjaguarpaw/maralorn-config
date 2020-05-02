{ config, ... }:
let
  inherit (config.m-0) hosts;
  inherit (config.m-0.private) monitoring-guest-pw monitoring-pw;
  makeProbe = module: targets: {
    job_name = "blackbox-${module}";
    metrics_path = "/probe";
    params = { module = [ module ]; };
    static_configs = [{ inherit targets; }];
    relabel_configs = [
      {
        source_labels = [ "__address__" ];
        target_label = "__param_target";
      }
      {
        source_labels = [ "__param_target" ];
        target_label = "instance";
      }
      {
        target_label = "__address__";
        replacement = "localhost:9115";
      } # The blackbox exporter's real hostname:port.
    ];
  };
  watchNixpkgsPackage = name: {
    job_name = "nixpkgs-${name}";
    metrics_path = "/job/${name}/prometheus";
    scheme = "https";
    scrape_interval = "1h";
    static_configs = [{ targets = [ "hydra.nixos.org" ]; }];
  };
  watchNixpkgsHaskellPackage = name: [
    (watchNixpkgsPackage
      "nixpkgs/haskell-updates/haskellPackages.${name}.x86_64-linux")
    (watchNixpkgsPackage
      "nixos/release-20.03/nixpkgs.haskellPackages.${name}.x86_64-linux")
  ];
in {
  services = {
    nginx = {
      enable = true;
      virtualHosts."monitoring.maralorn.de" = {
        enableACME = true;
        basicAuth.maralorn = monitoring-pw;
        basicAuth.guest = monitoring-guest-pw;
        forceSSL = true;
        locations."/" = { proxyPass = "http://localhost:9090"; };
      };
      virtualHosts."alerts.maralorn.de" = {
        enableACME = true;
        basicAuth.maralorn = monitoring-pw;
        forceSSL = true;
        locations."/" = { proxyPass = "http://localhost:9093"; };
      };
      virtualHosts."stats.maralorn.de" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:3000/";
      };
    };
    grafana = {
      enable = true;
      auth.anonymous.enable = true;
      users = {
        allowOrgCreate = false;
        allowSignUp = false;
      };
      provision = {
        enable = true;
        datasources = [{
          access = "proxy";
          name = "prometheus";
          type = "prometheus";
          url = "http://localhost:9090";
        }];
        dashboards = [{
          name = "Static dashboards";
          options.path = ./grafana-dashboards;
        }];
      };
    };

    prometheus = {
      exporters = {
        blackbox = {
          enable = true;
          configFile = ./blackbox_rules.yml;
        };
      };
      alertmanager = {
        enable = true;
        listenAddress = "0.0.0.0";
        configuration = {
          "global" = {
            "smtp_smarthost" = "hera.m-0.eu:587";
            "smtp_from" = "alertmanager@m-0.eu";
            "smtp_auth_username" = "alertmanager@m-0.eu";
            "smtp_auth_password" = config.m-0.private.alertmanager-mail-pw;
          };
          "route" = {
            "group_by" = [ "alertname" "alias" ];
            "group_wait" = "30s";
            "group_interval" = "2m";
            "repeat_interval" = "4h";
            "receiver" = "team-admins";
          };
          "receivers" = [{
            "name" = "team-admins";
            "email_configs" = [{
              "to" = "monitoring@maralorn.de";
              "send_resolved" = true;
            }];
          }];
        };
      };
      enable = true;
      ruleFiles = [ ./rules.yml ];
      scrapeConfigs = [
        (makeProbe "tcp_connect" [ "hera.m-0.eu:25" "hera.m-0.eu:80" ])
        (makeProbe "tls_connect" [ "hera.m-0.eu:993" "hera.m-0.eu:443" ])
        (makeProbe "smtp_starttls" [ "hera.m-0.eu:587" ])
        (makeProbe "http" [ "http://localhost:9090" "http://localhost:9093" ])
        (makeProbe "https" [
          "https://blog.maralorn.de"
          "https://www.mathechor.de"
          "https://cloud.mathechor.de"
          "https://cloud.maralorn.de"
          "https://riot.maralorn.de"
          "https://wiki.vocalensemble-darmstadt.de"
          "https://cloud.vocalensemble-darmstadt.de"
          "https://www.vocalensemble-darmstadt.de"
          "https://matrix.maralorn.de"
        ])
        {
          job_name = "matrix";
          metrics_path = "/_synapse/metrics";
          static_configs = [{ targets = [ "localhost:9148" ]; }];
        }
        {
          job_name = "nodes";
          static_configs = map (entry: {
            targets = [ entry.host ];
            labels = { "name" = entry.name; };
          }) config.m-0.monitoring;
        }
      ] ++ (watchNixpkgsHaskellPackage "ghcide")
        ++ (watchNixpkgsHaskellPackage "brittany")
        ++ (watchNixpkgsHaskellPackage "releaser")
        ++ (watchNixpkgsHaskellPackage "hlint")
        ++ (watchNixpkgsHaskellPackage "cabal-fmt")
        ++ (watchNixpkgsHaskellPackage "relude")
        ++ (watchNixpkgsHaskellPackage "taskwarrior");
      alertmanagers =
        [{ static_configs = [{ targets = [ "localhost:9093" ]; }]; }];
    };
  };
}
