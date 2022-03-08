{config, ...}: let
  makeProbe = module: targets: {
    job_name = "blackbox ${module}";
    metrics_path = "/probe";
    params.module = [module];
    static_configs = [
      {
        inherit targets;
        labels.alert_type = "infrastructure";
      }
    ];
    relabel_configs = [
      {
        source_labels = ["job" "__address__"];
        target_label = "name";
        separator = " against ";
      }
      {
        source_labels = ["__address__"];
        target_label = "__param_target";
      }
      {
        source_labels = ["__param_target"];
        target_label = "instance";
      }
      {
        # The blackbox exporter's real hostname:port.
        target_label = "__address__";
        replacement = "localhost:9115";
      }
    ];
  };
in {
  services.prometheus = {
    exporters.blackbox = {
      enable = true;
      configFile = ./blackbox_rules.yml;
    };
    scrapeConfigs = [
      (makeProbe "tls_connect" ["hera.m-0.eu:993"])
      (makeProbe "smtp_starttls" [
        "hera.m-0.eu:587"
        "bach.vocalensemble-darmstadt.de:25"
        "hera.m-0.eu:25"
      ])
      (makeProbe "http" [
        "hera.m-0.eu:80"
        "home.vpn.m-0.eu"
      ])
      (makeProbe "https" [
        "https://hera.m-0.eu"

        "https://cloud.mathechor.de"
        "https://mathechor.de"
        "https://www.mathechor.de"

        "https://alerts.maralorn.de"
        "https://blog.maralorn.de"
        "https://ci.maralorn.de"
        "https://cloud.maralorn.de"
        "https://element.maralorn.de"
        "https://firefox-sync.maralorn.de"
        "https://fdroid.maralorn.de/index-v1.json"
        "https://matrix.maralorn.de"
        "https://monitoring.maralorn.de"
        "https://rpg.maralorn.de"
        "https://stats.maralorn.de"
        "https://tasks.maralorn.de"

        "https://wiki.vocalensemble-darmstadt.de"
        "https://lists.vocalensemble-darmstadt.de"
        "https://cloud.vocalensemble-darmstadt.de"
        "https://www.vocalensemble-darmstadt.de"
      ])
    ];
  };
}
