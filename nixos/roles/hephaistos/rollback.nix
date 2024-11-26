{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfgZfs = config.boot.zfs;
in
{
  systemd.shutdownRamfs = {
    contents."/etc/systemd/system-shutdown/zpool".source = lib.mkForce (
      pkgs.writeShellScript "zpool-sync-shutdown" ''
        ${cfgZfs.package}/bin/zfs rollback -r zroot@blank
        exec ${cfgZfs.package}/bin/zpool sync
      ''
    );
    storePaths = [ "${cfgZfs.package}/bin/zfs" ];
  };
}
