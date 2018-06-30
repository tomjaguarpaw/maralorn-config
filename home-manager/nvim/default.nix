pkgs: mkRust:
pkgs.neovim.override {
  vimAlias = true;
  withPython3 = true;
  configure = {
    customRC = if mkRust then ''
      let $RUST_SRC_PATH="${pkgs.rustPlatform.rustcSrc}"
      let g:rustfmt_command = "${pkgs.cargo}/bin/cargo fmt"
      let g:racer_cmd = "${pkgs.rustracer}/bin/racer"
      let g:syntastic_rust_rustc_exe = '${pkgs.cargo}/bin/cargo check'
      let g:rustfmt_autosave = 1
      let g:syntastic_rust_checkers = [ 'cargo']
      let g:rust_recommend_style = 1
      let g:rust_fold =1
      ${builtins.readFile ./vimrc}
   '' else builtins.readFile ./vimrc;
    packages.myVimPackage = with pkgs.vimPlugins; {
      start = [
        vim-nix
        ctrlp
        vimtex
        Syntastic
        UltiSnips
        airline
        rust-vim
        fugitive
        airline
        vim-trailing-whitespace
        vim-polyglot
        nvim-cm-racer
        nvim-completion-manager
        vim-pandoc
        nerdcommenter
        vim-signify
      ];
    };
  };
}
