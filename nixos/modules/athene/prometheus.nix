{ config, lib, ... }:
let
  inherit (config.m-0) virtualHosts;
  targets = [
    # Athene
    {
      name = "athene";
      host = "athene:9100";
    }
    {
      name = "athene user services";
      host = "athene:9558";
    }
    {
      name = "nginx on athene";
      host = "hera:9113";
    }
    {
      name = "home assistant on athene";
      metrics_path = "/api/prometheus";
      host = "[::1]:8123";
    }
    # Hera
    {
      name = "hera";
      host = "hera:9100";
    }
    {
      name = "hera user services";
      host = "hera:9558";
    }
    {
      name = "nginx on hera";
      host = "hera:9113";
    }
    {
      name = "headscale on hera";
      host = "hera:9098";
    }
    {
      name = "postfix on hera";
      host = "hera:9154";
    }
    {
      name = "matrix-synapse on hera";
      metrics_path = "/_synapse/metrics";
      host = "hera:9148";
    }
    # Apollo
    {
      name = "apollo";
      host = "apollo:9100";
      flaky = true;
    }
    {
      name = "apollo user services";
      host = "apollo:9558";
      flaky = true;
    }
    # Hephaistos
    {
      name = "hephaistos";
      host = "hephaistos:9100";
      flaky = true;
    }
    {
      name = "hephaistos user services";
      host = "hephaistos:9558";
      flaky = true;
    }
    # Zeus
    {
      name = "zeus";
      host = "zeus:9100";
      flaky = true;
    }
    {
      name = "zeus user services";
      host = "zeus:9558";
      flaky = true;
    }
    # Vocalensemble
    {
      name = "bach.ved";
      host = "bach.vocalensemble-darmstadt.de:9100";
    }
    {
      name = "postfix on bach.ved";
      host = "bach.vocalensemble-darmstadt.de:9154";
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
