{
  folders ? [],
  subfolders ? [],
}: {
  pkgs,
  lib,
  ...
}: let
  printFolderSizes = pkgs.writeShellScript "print-folder-sizes" ''
  '';
  textfilesDir = "/var/cache/prometheus-textfiles";
in {
  services.prometheus.exporters.node.extraFlags = ["--collector.textfile.directory /var/cache/prometheus-textfiles"];
  systemd = {
    services.folder-size-exporter = {
      description = "Write folder size for promtext exporter";
      serviceConfig.Type = "oneshot";
      script = ''
        mkdir -p ${textfilesDir}
        du -xd0 \
          ${lib.concatStringsSep " " folders} \
          ${lib.concatMapStringsSep " " (x: "${x}/*") subfolders} \
          | sed 's/^\([[:digit:]]\+\)[[:blank:]]\+\(.*\)$/folder_size{folder="\2"} \1/' \
          > ${textfilesDir}/folder-sizes.prom.new
        mv ${textfilesDir}/folder-sizes.prom.new ${textfilesDir}/folder-sizes.prom
        chown -R node-exporter ${textfilesDir}
      '';
    };
    timers.folder-size-exporter = {
      wantedBy = ["timers.target"];
      timerConfig.OnCalendar = "2,8,14,20:00";
    };
  };
}
