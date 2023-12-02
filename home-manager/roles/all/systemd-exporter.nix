{pkgs, ...}:
{
  systemd.user = {
    services.prometheus-systemd-exporter = {
      Unit.Description = "Prometheus systemd exporter";
      Service = {
        ExecStart = "${pkgs.prometheus-systemd-exporter}/bin/systemd_exporter --systemd.collector.user";
      };
      Install.WantedBy = ["default.target"];
    };
  };
}
