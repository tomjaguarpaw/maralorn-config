neovim:
neovim.override {
  vimAlias = true;
  withPython3 = true;
  configure = {
    customRC = builtins.readFile ./vimrc;
    packages.myVimPackage = {
      start = builtins.attrValues {
      	inherit ((import <nixpkgs> {}).vimPlugins)
        vim-nix
        vimtex
        airline
        rust-vim
        fugitive
        vim-trailing-whitespace
        vim-pandoc
        vim-pandoc-syntax
        haskell-vim;
      };
    };
  };
}
