n:
n.override (let
  pkgs = import <nixpkgs> { };
  inherit (import ../../lib) sources colors;
  tabnine = sources.tabnine-vim;
in {
  configure = {
    customRC = ''
      set rtp+=${tabnine}
      ${builtins.readFile ./vimrc}
      hi Normal ctermbg=NONE
    '';
    packages.myVimPackage = {
      start = builtins.attrValues {
        inherit (pkgs.vimPlugins)
          vim-nix vimtex airline rust-vim fugitive vim-trailing-whitespace
          vim-airline-themes vim-pandoc vim-pandoc-syntax haskell-vim
          vim-autoformat vim-ledger papercolor-theme nerdcommenter vundle
          coc-nvim coc-python coc-rls coc-yaml coc-vimtex coc-css coc-json
          coc-html coc-git coc-tabnine;
      };
    };
  };
})
