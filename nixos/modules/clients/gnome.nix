{ lib, pkgs, ... }: {
  programs = {
    seahorse.enable = lib.mkForce false;
    dconf.enable = true;
  };
  services = {
    xserver = {
      enable = true;
      displayManager = {
        autoLogin = {
          enable = true;
          user = "maralorn";
        };
        gdm.enable = true;
      };
      desktopManager.gnome.enable = true;
    };
    gnome = {
      evolution-data-server.enable = lib.mkForce false;
      gnome-keyring.enable = lib.mkForce false;
      at-spi2-core.enable = lib.mkForce false;
      tracker.enable = false;
      tracker-miners.enable = false;
      gnome-online-miners.enable = lib.mkForce false;
      core-utilities.enable = lib.mkForce false;
    };
  };
  environment.gnome.excludePackages = [ pkgs.orca pkgs.gnome-tour ];
}
