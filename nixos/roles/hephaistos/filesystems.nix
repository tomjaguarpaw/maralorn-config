{
  imports = [
    (import ./_disko-config.nix {
      device = "/dev/disk/by-id/nvme-SAMSUNG_MZVL22T0HBLB-00BL7_S64SNA0W301677";
    })
  ];
  fileSystems = {
    "/disk/persist".neededForBoot = true;
    "/disk/volatile".neededForBoot = true;
  };

  services = {
    snapper = {
      configs.persist = {
        SUBVOLUME = "/disk/persist";
        TIMELINE_MIN_AGE = "3600";
        TIMELINE_LIMIT_WEEKLY = "4";
        TIMELINE_LIMIT_MONTHLY = "1";
        TIMELINE_LIMIT_YEARLY = "0";
        TIMELINE_CREATE = true;
        TIMELINE_CLEANUP = true;
      };
      cleanupInterval = "15m";
      snapshotInterval = "*:00/3:00";
    };
  };
}
