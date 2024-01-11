{ pkgs, ... }:
{
  home.packages = builtins.attrValues pkgs.mode-scripts;
}
