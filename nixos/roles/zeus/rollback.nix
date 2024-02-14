{ pkgs, ... }:
{
  boot.initrd.systemd.services.rollback = {
    description = "Delete everything but /nix and /disk on the root filesystem to get a fresh nixos install.";
    wantedBy = [ "initrd-root-fs.target" ];
    after = [ "sysroot.mount" ];
    unitConfig.DefaultDependencies = "no";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writers.writeHaskell "rollback" {
        libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
      } (builtins.readFile ./sysroot-rollback.hs);
    };
  };
}
