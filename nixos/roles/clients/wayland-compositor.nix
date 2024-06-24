{ pkgs, config, ... }:
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "launch-wayland" (
      if config.programs.hyprland.enable then
        "exec Hyprland |& ts '[%Y-%m-%d %H:%M:%S]' >> /run/user/$UID/hyprland.log"
      else
        "XDG_CURRENT_DESKTOP=river exec river |& ts '[%Y-%m-%d %H:%M:%S]' >> /run/user/$UID/river.log"
    ))
    pkgs.ddcutil
  ];
  hardware.i2c.enable = true;
  programs.river.enable = !config.programs.hyprland.enable;
  programs.hyprland.enable = config.m-0.hyprland;
  services.greetd = {
    enable = true;
    vt = 2;
  };
}
