n:
n.override {
  vimAlias = true;
  withPython3 = true;
  configure = {
    customRC = builtins.readFile ./vimrc;
    packages.myVimPackage = {
      start = builtins.attrValues {
        inherit ((import <nixpkgs> { }).vimPlugins)
          vim-nix vimtex airline rust-vim fugitive vim-trailing-whitespace
          vim-airline-themes vim-pandoc vim-pandoc-syntax haskell-vim
          vim-autoformat vim-ledger papercolor-theme nerdcommenter;
        inherit ((import <unstable> { }).vimPlugins)
          coc-nvim coc-python coc-rls coc-yaml coc-vimtex coc-css coc-json
          coc-html coc-git;
      };
    };
  };
}
