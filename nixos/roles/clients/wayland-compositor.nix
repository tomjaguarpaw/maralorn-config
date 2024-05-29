{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "launch-wayland" "XDG_CURRENT_DESKTOP=river exec river |& ts '[%Y-%m-%d %H:%M:%S]' >> /run/user/$UID/river.log")
    pkgs.ddcutil
  ];
  hardware.i2c.enable = true;
  services.greetd = {
    enable = true;
    vt = 2;
  };
  programs.sway.enable = true; # For swaylock pam files …
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
