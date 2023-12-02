{pkgs, ...}:
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "launch-wayland" ''
      XDG_CURRENT_DESKTOP=river exec river &>> /run/user/$UID/river.log
    '')
  ];
  services.greetd.enable = true;
  programs.sway.enable = true; # For swaylock pam files …
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
