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
in {
  systemd.tmpfiles.rules = let cfg = config.services.gitolite;
  in [ "Z ${cfg.dataDir}/.ssh 0600 ${cfg.user} ${cfg.group} - -" ];
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
      ];
      alertmanagers =
        [{ static_configs = [{ targets = [ "localhost:9093" ]; }]; }];
    };
  };
}
