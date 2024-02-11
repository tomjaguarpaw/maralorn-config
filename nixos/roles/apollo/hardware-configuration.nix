{ lib, modulesPath, ... }:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  boot = {
    initrd = {
      luks.devices."nixos".device = "/dev/disk/by-uuid/78acaebe-952a-43b1-acc8-66c35a60577e";
      availableKernelModules = [
        "xhci_pci"
        "nvme"
        "usb_storage"
        "sd_mod"
        "rtsx_pci_sdmmc"
      ];
    };
    kernelModules = [ "kvm-intel" ];
    supportedFilesystems = [ "exfat" ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/ce5b0ac6-6eaf-45a6-b6c8-bd4958caf335";
      fsType = "btrfs";
      options = [
        "compress=zstd"
        "autodefrag"
        "noatime"
      ];
    };
    "/efi" = {
      device = "/dev/disk/by-uuid/256B-9B5C";
      fsType = "vfat";
    };
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  nix.settings.max-jobs = lib.mkDefault 8;
}
