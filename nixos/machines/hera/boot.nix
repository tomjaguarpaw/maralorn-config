{
  m-0.server.initSSHKey = "/var/boot-ssh-key";

  boot = {
    loader.grub = {
      enable = true;
      device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0";
    };
    supportedFilesystems = [ "exfat" ];
    kernelParams = [ "ip=213.136.94.190::213.136.94.1:255.255.255.0:hera" ];
    initrd.luks.devices.root = {
      device = "/dev/disk/by-uuid/536fe284-36f2-425c-b0c5-a737280f9470";
      preLVM = true;
      allowDiscards = true;
    };
  };
}
