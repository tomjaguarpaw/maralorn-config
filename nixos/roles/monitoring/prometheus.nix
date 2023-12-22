{ config, lib, ... }:
{
  services = {
    prometheus = {
      enable = true;
      extraFlags = [
        "--query.lookback-delta=180m"
        "--storage.tsdb.retention.time=720d"
      ];
      ruleFiles = [ ./rules.yml ];
      scrapeConfigs =
        let
          alert_type = "infrastructure";
        in
        [
          (
            let
              name = "matrix-synapse";
            in
            {
              job_name = name;
              metrics_path = "/_synapse/metrics";
              static_configs = [
                {
                  targets = [ "localhost:9148" ];
                  labels = {
                    inherit name;
                    inherit alert_type;
                  };
                }
              ];
            }
          )
        ]
        ++
          map
            (
              entry:
              let
                inherit (entry) name;
              in
              {
                job_name = name;
                static_configs = [
                  {
                    targets = [ entry.host ];
                    labels = {
                      inherit name;
                      inherit alert_type;
                      inContainer = lib.boolToString entry.container;
                      flaky = lib.boolToString entry.flaky;
                    };
                  }
                ];
              }
            )
            config.m-0.monitoring;
    };
  };
}
