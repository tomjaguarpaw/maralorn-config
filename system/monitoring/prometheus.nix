{ config, ... }: {
  services = {
    prometheus = {
      enable = true;
      extraFlags =
        [ "--query.lookback-delta 2h" "--storage.tsdb.retention.time 720d" ];
      exporters = {
        blackbox = {
          enable = true;
          configFile = ./blackbox_rules.yml;
        };
      };
      ruleFiles = [ ./rules.yml ];
      scrapeConfigs = [
        {
          job_name = "matrix-synapse";
          metrics_path = "/_synapse/metrics";
          static_configs = [{
            targets = [ "localhost:9148" ];
            labels = {
              __myjobname = "collected-exporters";
              name = "matrix-synapse";
              alert_type = "infrastructure";
            };
          }];
          relabel_configs = [{
            source_labels = [ "__myjobname" ];
            target_label = "job";
          }];
        }
        {
          job_name = "collected-exporters";
          static_configs = map (entry: {
            targets = [ entry.host ];
            labels = {
              inherit (entry) name;
              alert_type = "infrastructure";
            };
          }) config.m-0.monitoring;
        }
      ];
    };
  };
}
