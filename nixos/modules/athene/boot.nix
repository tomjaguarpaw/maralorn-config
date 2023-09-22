{
  m-0.server.initSSHKey = "/disk/persist/boot-ssh-key";
  boot = {
    kernelParams = [ "ip=192.168.178.58::192.168.178.1:255.255.255.0:athene" ];
    loader = {
      efi.efiSysMountPoint = "/efi";
      grub = {
        efiSupport = true;
        efiInstallAsRemovable = true;
        mirroredBoots = [ {
          devices = [ "nodev" ];
          path = "/efi";
        } ];
      };
    };
  };
}
