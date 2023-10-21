{
  services.udiskie = {
    enable = true;
    automount = false;
    tray = "never";
  };
  systemd.user.services.udiskie.Service = {
    Restart = "always";
    RestartSec = "10s";
  };
}
