pkgs: mkRust:
pkgs.neovim.override {
  vimAlias = true;
  withPython3 = true;
  configure = {
    customRC = if mkRust then ''
      let $RUST_SRC_PATH="${pkgs.rustPlatform.rustcSrc}"
      let g:rustfmt_command = "${pkgs.rustfmt}/bin/rustfmt"
      let g:racer_cmd = "${pkgs.rustracer}/bin/racer"
      let g:rustfmt_autosave = 1
      let g:syntastic_rust_checkers = [ 'cargo']
      let g:rust_recommend_style = 1
      let g:rust_fold =1
      ${builtins.readFile ./vimrc}
   '' else builtins.readFile ./vimrc;
    packages.myVimPackage = with pkgs.vimPlugins; {
      start = [
        vim-nix
        vimtex
        airline
        rust-vim
        fugitive
        vim-trailing-whitespace
        vim-pandoc
        haskell-vim
        vim-hindent
      ];
    };
  };
}
