{
  boot = {
    initrd.network.ssh.enable = true;
    loader = {
      efi.efiSysMountPoint = "/efi";
      grub = {
        efiSupport = true;
        efiInstallAsRemovable = true;
        mirroredBoots = [{
          devices = [ "nodev" ];
          path = "/efi";
        }];
      };
    };
  };
}
