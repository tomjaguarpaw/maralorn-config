{
  boot.kernelParams = [ "mitigations=off" ];
  console.keyMap = "neo";
  services = {
    fwupd.enable = true;
    upower.enable = true;
    fstrim.enable = true;
  };
  systemd.services.fwupd-refresh.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "30s";
  };
}
