{
  boot = {
    kernelParams = [ "amdgpu.cik_support=1" ];
    initrd.kernelModules = [ "amdgpu" ]; # For earlier and better framebuffer
  };
}
