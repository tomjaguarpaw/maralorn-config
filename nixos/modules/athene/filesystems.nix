{
  imports = [
    (import ./_disko-config.nix {
      device = "/dev/disk/by-id/ata-Samsung_SSD_870_QVO_4TB_S5STNJ0W103706V";
    })
  ];
  fileSystems = {
    "/disk/persist".neededForBoot = true;
    "/disk/volatile".neededForBoot = true;
  };
}
