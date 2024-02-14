{ pkgs, ... }:
{
  m-0.server.initrd-ssh = {
    key = "/disk/persist/boot-ssh-key";
    networkingModules = [ "igb" ];
  };
  boot = {
    kernelParams = [ "amdgpu.cik_support=1" ];
    initrd = {
      kernelModules = [ "amdgpu" ]; # For earlier and better framebuffer
      systemd = {
        enable = true;
        services.rollback = {
          description = "Delete everything but /nix and /disk on the root filesystem to get a fresh nixos install.";
          wantedBy = [ "initrd-root-fs.target" ];
          after = [ "sysroot.mount" ];
          unitConfig.DefaultDependencies = "no";
          serviceConfig = {
            Type = "oneshot";
            ExecStart = pkgs.writeHaskell "rollback" {
              libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
            } (builtins.readFile ./sysroot-rollback.hs);
          };
        };
      };
    };
  };
}
