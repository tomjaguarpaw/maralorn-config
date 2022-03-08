{pkgs, ...}: {
  home.packages = [pkgs.zotero];
  programs.texlive = {
    enable = true;
    extraPackages = p: {inherit (p) scheme-full;};
  };
}
