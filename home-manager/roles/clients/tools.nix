{pkgs, ...}:
{
  home.packages = [
    pkgs.graphviz
    pkgs.typst
    pkgs.texlive.combined.scheme-full
  ];
}
