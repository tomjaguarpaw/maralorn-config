{pkgs, ...}:
{
  home.packages = [pkgs.zotero];
  programs.texlive = {
    enable = false;
    extraPackages = p: {inherit (p) scheme-medium;};
  };
}
