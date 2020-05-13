{ pkgs, ... }: {
  home.packages = [ pkgs.zotero ];
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        scheme-medium pdfjam latexmk collection-latexextra
        collection-bibtexextra collection-luatex collection-mathscience
        collection-fontsextra dvipng;
    };
  };
}
