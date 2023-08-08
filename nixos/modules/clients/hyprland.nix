{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "launch-hyprland" ''
      rm -rf /tmp/hypr
      exec Hyprland &>> /run/user/$UID/hyprland.log
    '')
  ];
  services.greetd.enable = true;
  programs = {
    hyprland.enable = true;
    sway.enable = true; # For swaylock pam files …
  };
}
