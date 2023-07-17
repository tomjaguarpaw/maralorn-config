{ pkgs, config, ... }:
{
  home.sessionVariables.TERMINAL = "${pkgs.foot}/bin/foot";
  home.packages = [
    (pkgs.recursiveLinkFarm "fake-gnome-terminal" {
      "bin/gnome-terminal" = config.home.sessionVariables.TERMINAL;
    })
  ];
  programs.foot = {
    settings = {
      main = {
        font = "Symbols Nerd Font Mono:pixelsize=12,CozetteVector:pixelsize=12";
        include =
          (pkgs.runCommandLocal "foot-theme" { } ''
            cat ${pkgs.foot.themes}/share/foot/themes/catppuccin > $out
            echo -e "background=000000\nalpha=0.9" >> $out
          '').outPath;
      };
      csd = {
        preferred = "client";
        size = "0";
        border-width = "1";
        color = "ff${config.m-0.colors.accent}";
      };
      mouse = {
        hide-when-typing = "yes";
      };
      tweak.font-monospace-warn = "no";
    };
    enable = true;
  };
}
