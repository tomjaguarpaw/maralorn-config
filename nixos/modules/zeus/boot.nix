{
  m-0.server.initrd-ssh = {
    key = "/disk/persist/boot-ssh-key";
    networkingModules = ["igb"];
  };
  boot = {
    kernelParams = ["amdgpu.cik_support=1"];
    initrd = {
      luks.devices."crypted-nixos" = {
        # device defined in hardware-configuration.nix
        allowDiscards = true;
      };
      kernelModules = [
        "amdgpu" # For earlier and better framebuffer
      ];
    };
  };
}
