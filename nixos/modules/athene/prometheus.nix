{ config, lib, ... }:
let
  inherit (config.m-0) virtualHosts;
  targets = [
    {
      name = "apollo";
      host = "apollo:9100";
      flaky = true;
    }
    {
      name = "apollo-user";
      host = "apollo:9558";
      flaky = true;
    }
    {
      name = "hera-user";
      host = "hera:9558";
    }
    {
      name = "zeus";
      host = "zeus:9100";
      flaky = true;
    }
    {
      name = "zeus-user";
      host = "zeus:9558";
      flaky = true;
    }
    {
      name = "athene";
      host = "athene:9100";
    }
    {
      name = "hera";
      host = "hera:9100";
    }
    {
      name = "ved server";
      host = "bach.vocalensemble-darmstadt.de:9100";
    }
    {
      name = "ved postfix";
      host = "bach.vocalensemble-darmstadt.de:9154";
    }
    {
      name = "hera-nginx";
      host = "hera:9113";
    }
    {
      name = "hera-headscale";
      host = "hera:9098";
    }
    {
      name = "postfix on hera";
      host = "hera:9154";
    }
    {
      metrics_path = "/_synapse/metrics";
      name = "matrix-synapse on hera";
      host = "hera:9148";
    }
  ];
in
{
  services = {
    nginx.virtualHosts.${virtualHosts.monitoring}.locations."/".proxyPass = "http://localhost:9090";
    prometheus = {
      enable = true;
      extraFlags = [
        "--query.lookback-delta=180m"
        "--storage.tsdb.retention.time=180d"
      ];
      ruleFiles = [ ./rules.yml ];
      scrapeConfigs =
        let
          alert_type = "infrastructure";
        in
        map
          (
            entry:
            let
              inherit (entry) name;
            in
            {
              job_name = name;
              metrics_path = entry.metrics_path or null;
              static_configs = [
                {
                  targets = [ entry.host ];
                  labels = {
                    inherit name;
                    inherit alert_type;
                    flaky = lib.boolToString entry.flaky or false;
                  };
                }
              ];
            }
          )
          targets;
    };
  };
}
