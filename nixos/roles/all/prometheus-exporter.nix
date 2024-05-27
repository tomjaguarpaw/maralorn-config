{ pkgs, ... }:
{
  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = [
      "systemd"
      "logind"
    ];
    disabledCollectors = [ "timex" ];
  };

  systemd.services.prometheus-nixos-exporter = {
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    path = [
      pkgs.nix
      pkgs.bash
    ];
    serviceConfig = {
      Restart = "always";
      RestartSec = "30s";
      ExecStart = "${pkgs.prometheus-nixos-exporter}/bin/prometheus-nixos-exporter";
    };
    unitConfig.StartLimitIntervalSec = "3m";
  };
}
