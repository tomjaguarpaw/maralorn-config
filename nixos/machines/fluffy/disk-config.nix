{device, ...}: {
  disko.devices.disk.system = {
    type = "disk";
    inherit device;
    content = {
      type = "table";
      format = "gpt";
      partitions = [
        {
          name = "efi-system-partition";
          start = "1MiB";
          end = "1GiB";
          fs-type = "fat32";
          bootable = true;
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/efi";
          };
        }
        {
          name = "swap";
          start = "1GiB";
          end = "17GiB";
          content = {
            type = "swap";
            randomEncryption = true;
          };
        }
        {
          name = "data-disk";
          start = "17GiB";
          end = "100%";
          content = {
            type = "luks";
            name = "crypted-nixos";
            extraOpenArgs = ["--allow-discards"];
            content = {
              type = "btrfs";
              extraArgs = ["-f"]; # Override existing partition
              subvolumes = {
                # Mountpoints inferred from subvolume name
                "/disk" = {
                  mountOptions = ["compress=zstd"];
                };
                "/disk/persist" = {
                  mountOptions = ["compress=zstd"];
                };
                "/disk/volatile" = {
                  mountOptions = ["compress=zstd"];
                };
                "/nix" = {
                  mountOptions = ["compress=zstd" "noatime"];
                };
              };
            };
          };
        }
      ];
    };
  };
}
