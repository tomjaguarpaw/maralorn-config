{ device, ... }:
{
  disko.devices.disk.system = {
    type = "disk";
    inherit device;
    content = {
      type = "gpt";
      partitions = {
        ESP = {
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
          type = "8200"; # Linux swap
          size = "32G";
          priority = 2000; # This is between the default priority 1000 and the 100% priority 9001
          content = {
            type = "swap";
            randomEncryption = true;
          };
        };
        system-vault = {
          type = "8309"; # Linux LUKS
          size = "100%"; # Uses rest of disk
          content = {
            type = "luks";
            # LUKS2 defaults on nixos are sane and use argon2id
            name = "system";
            settings.allowDiscards = true;
            content = {
              type = "filesystem";
              format = "bcachefs";
              mountpoint = "/";
              mountOptions = [
                "defaults"
                "noatime"
              ];
              extraArgs = [ "--compression=zstd" ];
            };
          };
        };
      };
    };
  };
}
