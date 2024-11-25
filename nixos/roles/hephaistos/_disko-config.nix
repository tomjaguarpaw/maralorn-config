{ device, ... }:
{
  disko.devices = {
    disk.nixos-hephaistos = {
      type = "disk";
      inherit device;
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            priority = 1;
            type = "EF00"; # EFI system partition
            size = "1G";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/efi";
              mountOptions = [
                "defaults"
                "noatime"
              ];
            };
          };
          swap = {
            priority = 2;
            type = "8200"; # Linux swap
            size = "32G";
            content = {
              type = "swap";
              randomEncryption = true;
            };
          };
          system-vault = {
            priority = 3;
            type = "8309"; # Linux LUKS
            size = "100%"; # Uses rest of disk
            content = {
              type = "luks";
              # LUKS2 defaults on nixos are sane and use argon2id
              name = "crypted-root-fs";
              extraOpenArgs = [ "--allow-discards" ];
              content = {
                type = "zfs";
                pool = "zroot";
                subvolumes = {
                  "/persist" = {
                    mountOptions = [ "compress=zstd" ];
                    mountpoint = "/disk/persist";
                  };
                  "/volatile" = {
                    mountOptions = [ "compress=zstd" ];
                    mountpoint = "/disk/volatile";
                  };
                  "/nix" = {
                    mountpoint = "/nix";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                };
              };
            };
          };
        };
      };
    };
    zpool.zroot = {
      type = "zpool";
      options.cachefile = "none";
      rootFsOptions = {
        compression = "zstd";
        "com.sun:auto-snapshot" = "false";
      };

      mountpoint = "/";
      postCreateHook = "zfs list -t snapshot -H -o name | grep -E '^zroot@blank$' || zfs snapshot zroot@blank";

      datasets = {
        zfs_nix = {
          type = "zfs_fs";
          mountpoint = "/nix";
        };
        zfs_persist = {
          type = "zfs_fs";
          mountpoint = "/disk/persist";
          options."com.sun:auto-snapshot" = "true";
        };
        zfs_volatile = {
          type = "zfs_fs";
          mountpoint = "/disk/volatile";
        };
      };

    };
  };
}
