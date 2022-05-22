{pkgs, ...}: let
  config = [
    {
      "path" = "/";
      explode_depth = 1;
      "sum_remaining_subfolders" = true;
    }
  ];
in {
  systemd.services.prometheus-folder-size-exporter = {
    after = ["network-online.target"];
    wants = ["network-online.target"];
    description = "Prometheus folder size exporter";
    wantedBy = ["multi-user.target"];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.prometheus-folder-size-exporter}/bin/prometheus_folder_size_exporter -b 21600 -i ${builtins.toFile "prometheus-folder-size-exporter-config.json" (builtins.toJSON config)} -p 9974";
    };
  };
}
