{ lib, pkgs, config, ... }:
with lib; {

  options.m-0.latex.enable = mkEnableOption "Latex";

  config = mkIf config.m-0.latex.enable {
    programs = {
      texlive = {
        enable = true;
        extraPackages = tpkgs: {
          inherit (tpkgs)
            scheme-medium pdfjam latexmk collection-latexextra
            collection-bibtexextra collection-luatex collection-mathscience
            collection-fontsextra dvipng;
        };
      };
    };
  };

}
