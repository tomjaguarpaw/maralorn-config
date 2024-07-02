{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "launch-wayland" "exec Hyprland |& ts '[%Y-%m-%d %H:%M:%S]' >> /run/user/$UID/hyprland.log")
    pkgs.ddcutil
  ];
  hardware.i2c.enable = true;
  programs.hyprland.enable = true;
  services.greetd = {
    enable = true;
    vt = 2;
  };
}
