{ lib, config, ... }:
let secretsFile = "/var/lib/luks-secret/key";
in {
  boot = {
    initrd = {
      luks.devices."nixos" = {
        fallbackToPassword = true;
        keyFile = secretsFile;
      };
      # copy the secret into the additional initramfs. `null` means same path
      secrets."${secretsFile}" = null;
    };
  };
}
