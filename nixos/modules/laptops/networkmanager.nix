{pkgs, ...}:
{
  environment.systemPackages = [
    pkgs.networkmanagerapplet # For when the gnome dialog sucks in asking for a wifi password.
  ];

  networking.networkmanager.enable = true;

  systemd = {
    network.wait-online.enable = false;
    services.NetworkManager-wait-online.enable = false;
  };
}
