{ pkgs, ... }:
{
  systemd.user = {
    services.prometheus-systemd-exporter = {
      Unit.Description = "Prometheus systemd exporter";
      Service = {
        ExecStart = "${pkgs.prometheus-systemd-exporter}/bin/prometheus-systemd-exporter --collector.user";
      };
    };
  };
}
