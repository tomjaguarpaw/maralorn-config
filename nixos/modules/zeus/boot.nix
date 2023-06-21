{
  boot = {
    loader = {
      efi = { efiSysMountPoint = "/boot/efi"; };
      grub = {
        # Enabled by default
        device = "nodev"; # Don‘t write masterboot under efi
        efiInstallAsRemovable =
          true; # Make loader discoverable by filename on efidisk without needing to write efivars to system
        efiSupport = true;
        enableCryptodisk = true;
        backgroundColor = "#000000";
      };
    };
    kernelParams = [ "amdgpu.cik_support=1" ];
    initrd = {
      luks.devices."crypted-nixos" = {
        # device defined in hardware-configuration.nix
        allowDiscards = true;
        keyFile = "/diskkey.bin";
      };
      kernelModules = [
        "amdgpu" # For earlier and better framebuffer
      ];
      secrets = {
        "diskkey.bin" =
          "/disk/persist/diskkey.bin"; # Key can live on crypted disk, is copied to initrd on install
      };
    };
  };
}
