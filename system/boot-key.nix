{ lib, config, ... }:
let
  secretsFile = "/var/lib/luks-secret/key";
  secretsInitrd = "/boot/grub/secrets-initrd.gz";
in {
  boot.initrd.luks.devices."nixos" = {
    fallbackToPassword = true;
    keyFile = secretsFile;
  };
  # copy the secret into the additional initramfs. `null` means same path
  boot.initrd.secrets."${secretsFile}" = null;
  boot.loader = {
    supportsInitrdSecrets = lib.mkForce true;
    grub.extraInitrd = secretsInitrd;
    grub.extraPrepareConfig = ''
      ${config.system.build.initialRamdiskSecretAppender}/bin/append-initrd-secrets ${secretsInitrd}
    '';
  };
}
