{device, ...}: {
  disko.devices = {
    disk.system = {
      type = "disk";
      inherit device;
      content = {
        type = "table";
        format = "gpt";
        partitions = [
          {
            name = "ESP";
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
            name = "system-vault";
            start = "17GiB";
            content = {
              type = "luks";
              name = "system";
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
      nodev."/" = {
        fsType = "tmpfs";
        mountOptions = [
          "defaults"
          "mode=755"
        ];
      };
    };
  };
}
