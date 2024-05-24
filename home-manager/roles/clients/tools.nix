{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      starship
      graphviz
      typst
      parallel
      ;
    inherit (pkgs.texlive.combined) scheme-full;
  };
}
