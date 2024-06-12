{
  imports = [
    (import ./_disko-config.nix {
      device = "/dev/disk/by-id/nvme-SAMSUNG_MZVL21T0HCLR-00B00_S676NF0R402858";
    })
  ];
}
