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
                type = "btrfs";
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
    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "defaults"
        "mode=755"
      ];
    };
  };
}
