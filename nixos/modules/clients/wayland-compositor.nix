{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "launch-wayland" ''
      exec river &>> /run/user/$UID/river.log
    '')
  ];
  services.greetd.enable = true;
  programs.sway.enable = true; # For swaylock pam files …
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
