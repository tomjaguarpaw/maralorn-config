{ config, ... }:
let
  inherit (config.m-0) hosts;
  inherit (config.m-0.private) monitoring-guest-pw monitoring-pw;
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
    };

    prometheus2 = {
      enable = true;
      ruleFiles = [ ./rules.yml ];
      scrapeConfigs = [
        {
          job_name = "blackbox";
          metrics_path = "/probe";
          params = { module = [ "http_2xx" ]; };
          static_configs = [{
            targets = [
              "https://blog.maralorn.de"
              "https://www.mathechor.de"
              "https://cloud.mathechor.de/login"
              "https://cloud.maralorn.de/login"
              "https://riot.maralorn.de"
              "https://wiki.vocalensemble-darmstadt.de"
              "https://intern.vocalensemble-darmstadt.de"
              "https://www.vocalensemble-darmstadt.de"
              "https://matrix.maralorn.de"
              "http://localhost:9090"
              "http://localhost:9093"
            ];
          }];
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
        }
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
      alertmanagerURL = [ "localhost:9093" ];
    };
  };
}
