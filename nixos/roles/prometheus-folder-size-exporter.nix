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
      DynamicUser = true;
      Type = "simple";
      ExecStart = "${pkgs.prometheus-folder-size-exporter}/bin/prometheus-folder-size-exporter -b 60 -i ${builtins.toJSON config} -p 9974";
    };
  };
}
