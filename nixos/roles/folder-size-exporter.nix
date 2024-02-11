{
  folders ? [ "/" ],
}:
{ lib, ... }:
let
  textfilesDir = "/var/cache/prometheus-textfiles";
in
{
  services.prometheus.exporters.node.extraFlags = [
    "--collector.textfile.directory /var/cache/prometheus-textfiles"
  ];
  systemd = {
    services.folder-size-exporter = {
      description = "Write folder size for promtext exporter";
      serviceConfig.Type = "oneshot";
      script = ''
        mkdir -p ${textfilesDir}
        find ${lib.concatStringsSep " " folders} -maxdepth 1 \
          | grep -v "^\(${lib.concatStringsSep "\\|" folders}\)$" \
          | tr '\n' '\0' \
          | du --threshold=100M --summarize --files0-from=- --one-file-system \
          | sed 's/^\([[:digit:]]\+\)[[:blank:]]\+\(.*\)$/folder_size{folder="\2"} \1/' \
          > ${textfilesDir}/folder-sizes.prom.new
        mv ${textfilesDir}/folder-sizes.prom.new ${textfilesDir}/folder-sizes.prom
        chown -R node-exporter ${textfilesDir}
      '';
    };
    timers.folder-size-exporter = {
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "2,8,14,20:00";
    };
  };
}
