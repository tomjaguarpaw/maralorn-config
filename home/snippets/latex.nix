{ pkgs, ... }:
{
  programs = {
    texlive = {
      enable = true;
      extraPackages = tpkgs: {inherit (tpkgs)
        scheme-small
        latexmk
        collection-latexextra
        collection-bibtexextra
        collection-luatex
        collection-mathscience
        collection-fontsextra;
      };
    };
  };
  home.packages = [
    pgks.biber
  ];
}
