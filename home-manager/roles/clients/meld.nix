{ pkgs, ... }:
{
  home.packages = builtins.attrValues { inherit (pkgs) meld; };
  dconf.settings."org/gnome/meld" = {
    highlight-syntax = true;
    style-scheme = "cobalt";
  };
  programs.jujutsu.settings.ui = {
    merge-editor = "meld";
    diff-editor = "meld-3";
  };
}
