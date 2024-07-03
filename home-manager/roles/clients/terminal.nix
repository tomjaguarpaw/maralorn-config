{ pkgs, ... }:
{
  home.sessionVariables.TERMINAL = "${pkgs.foot}/bin/foot";
  programs.foot = {
    enable = true;
    settings = {
      main.font = "Symbols Nerd Font Mono:pixelsize=11,Spleen:pixelsize=11";
      mouse.hide-when-typing = "yes";
      tweak.font-monospace-warn = "no";
    };
  };
}
