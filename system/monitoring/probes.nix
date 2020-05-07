{ config, ... }:
let
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
  services.prometheus = {
    exporters = {
      blackbox = {
        enable = true;
        configFile = ./blackbox_rules.yml;
      };
    };
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
    ];
  };
}
