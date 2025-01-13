{ config, lib, ... }:
let
  inherit (config.m-0) virtualHosts;
  hostJobs = name: flaky: [
    {
      inherit name;
      host = "${name}:9100";
      inherit flaky;
    }
    {
      name = "nixos on ${name}";
      host = "${name}:9300";
      inherit flaky;
    }
    {
      name = "${name} user services";
      host = "${name}:9558";
      inherit flaky;
    }
    {
      name = "nginx on ${name}";
      host = "${name}:9113";
      inherit flaky;
    }
  ];

  targets =
    hostJobs "athene" false
    ++ [
      {
        name = "home assistant on athene";
        metrics_path = "/api/prometheus";
        host = "[::1]:8123";
      }
    ]
    ++ hostJobs "hera" false
    ++ [
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
      {
        name = "forgejo on hera";
        metrics_path = "/metrics";
        host = "code.maralorn.de";
      }
    ]
    ++ hostJobs "hephaistos" true
    ++ hostJobs "zeus" true
    ++ [
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
  environment.persistence.snapshoted.directories = [ "/var/lib/prometheus2" ];
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
        map (
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
        ) targets;
    };
  };
}
