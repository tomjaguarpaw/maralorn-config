let
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
        source_labels = [
          "job"
          "__address__"
        ];
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
in
{config, lib, ...}:
{
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
      (makeProbe "http" (
        ["hera.m-0.eu"]
        ++ lib.pipe (config.m-0.hosts.aliases.athene ++ config.m-0.hosts.aliases.hera) [
          (builtins.filter (name: name != "cache"))
          (map (name: "http://${name}.maralorn.de/"))
        ]
      ))
      (makeProbe "https" (
        [
          "https://hera.m-0.eu"

          "https://hochzeit.malte-und-clai.re"

          "https://wiki.vocalensemble-darmstadt.de"
          "https://lists.vocalensemble-darmstadt.de"
          "https://cloud.vocalensemble-darmstadt.de"
          "https://www.vocalensemble-darmstadt.de"
        ]
        ++ lib.pipe (config.m-0.hosts.aliases.athene ++ config.m-0.hosts.aliases.hera) [
          (builtins.filter (name: name != "cache"))
          (map (name: "https://${name}.maralorn.de/"))
        ]
      ))
    ];
  };
}
