{pkgs, ...}: {
  boot.kernelParams = ["mitigations=off"];
  services.udev.packages = [pkgs.chrysalis];
}
