_: {
  boot.kernelParams = [ "mitigations=off" ];
  console.keyMap = "neo";
  security.rtkit.enable = true;
  services = {
    fwupd.enable = true;
    upower.enable = true;
    fstrim.enable = true;
  };
}
