# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  fileSystems."/" = {
    device = "tmpfs";
    fsType = "tmpfs";
  };

  fileSystems."/disk" = {
    device = "/dev/disk/by-uuid/47552982-2abf-45c6-8c5c-d33091ce3f5a";
    fsType = "btrfs";
  };

  boot.initrd.luks.devices."crypted-nixos".device = "/dev/disk/by-uuid/2518e0e0-c263-40bc-b378-419832dc62cc";

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/47552982-2abf-45c6-8c5c-d33091ce3f5a";
    fsType = "btrfs";
    options = ["subvol=nix"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/47552982-2abf-45c6-8c5c-d33091ce3f5a";
    fsType = "btrfs";
    options = ["subvol=boot"];
  };

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/C41C-0C8E";
    fsType = "vfat";
  };

  swapDevices = [];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp34s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.m0wire.useDHCP = lib.mkDefault true;
  # networking.interfaces.tinc.cdark.net.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlo1.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  nix.settings.max-jobs = lib.mkDefault 12;
}
