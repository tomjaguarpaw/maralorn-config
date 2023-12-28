{ pkgs, ... }:
{
  home.sessionVariables.TERMINAL = "${pkgs.foot}/bin/foot";
  programs.foot = {
    settings = {
      main = {
        font = "Symbols Nerd Font Mono:pixelsize=11,Spleen:pixelsize=11";
        include =
          (pkgs.runCommandLocal "foot-theme" { } ''
            cat ${pkgs.foot.themes}/share/foot/themes/catppuccin > $out
            echo -e "background=000040\nalpha=0.9" >> $out
          '').outPath;
      };
      mouse = {
        hide-when-typing = "yes";
      };
      tweak.font-monospace-warn = "no";
    };
    enable = true;
  };
}
