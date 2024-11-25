{ pkgs, ... }:
{
  options."com.sun:auto-snapshot" = "true";
  boot.initrd.systemd.services.rollback = {
    description = "Rollback ZFS datasets to a pristine state";
    wantedBy = [ "initrd.target" ];
    after = [ "zfs-import-zroot.service" ];
    before = [ "sysroot.mount" ];
    path = [ pkgs.zfs ];
    unitConfig.DefaultDependencies = "no";
    serviceConfig.Type = "oneshot";
    script = ''
      set -ex
      zfs rollback -r zroot/root@blank && echo "rollback complete"
    '';
  };
}
