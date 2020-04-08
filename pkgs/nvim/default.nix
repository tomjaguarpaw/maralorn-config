n:
n.override (let
  pkgs = import <nixpkgs> { };
  inherit (import ../../lib) sources colors;
in {
  configure = {
    customRC = ''
      ${builtins.readFile ./vimrc}
      hi Normal ctermbg=NONE
    '';
    packages.myVimPackage = {
      start = builtins.attrValues {
        inherit (pkgs.vimPlugins)
        # rust-vim vimtex
        # coc-tabnine (TODO: Why doesn‘t it work?)
        # TODO: tabnine config in home-manager
        # TODO: tabnine lsp: nix, rust, pandoc/latex lsp? was noch?
          vim-pandoc vim-nix vimtex airline fugitive vim-trailing-whitespace
          haskell-vim vim-airline-themes vim-autoformat vim-ledger
          papercolor-theme nerdcommenter coc-nvim coc-python coc-rls coc-yaml
          coc-vimtex coc-css coc-json coc-html coc-git;
      };
    };
  };
})
