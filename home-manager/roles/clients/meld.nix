{ pkgs, ... }:
{
  home.packages = builtins.attrValues { inherit (pkgs) meld diffedit3; };
  dconf.settings."org/gnome/meld" = {
    highlight-syntax = true;
    style-scheme = "cobalt";
  };
  programs.jujutsu.settings.ui = {
    merge-editor = "diffedit3";
    diff-editor = "diffedit3";
  };
}
