{ pkgs, config, ... }:
let
  inherit (import ../../lib) unstable;
  myPkgs = import ../../pkgs;
  neovim = myPkgs.neovim.override {
    configure = {
      customRC = builtins.readFile ./vimrc;
      packages.myVimPackage = {
        start = builtins.attrValues {
          inherit (unstable.vimPlugins)
          # coc-tabnine (TODO: Why doesn‘t it work?)
          # TODO: tabnine config in home-manager
          # TODO: tabnine lsp: nix, rust, pandoc/latex lsp? was noch?
          # TODO: coc-explorer, coc-markdownlints are installed statefully right now

          # ===
          # Basic IDE plugins
            coc-nvim airline
            # same word highlighting when not supported by language
            coc-highlight
            # searches
            coc-fzf fzf-vim

            # general whitespace
            vim-trailing-whitespace vim-autoformat

            # snippets
            coc-snippets # integrates completion
            vim-snippets # provides standard snippets

            # Git
            coc-git # statusline, numberline and explorer infos
            fugitive # various git commands

            # Commenting and Uncommenting
            nerdcommenter

            # Theme
            papercolor-theme vim-airline-themes

            # ===
            # Languages
            # haskell syntax highlighting
            haskell-vim
            # nix syntax highlighting
            vim-nix
            # latex
            vimtex coc-vimtex # not sure if I need two
            # ledger
            vim-ledger
            # rust
            coc-rls
            # python
            coc-python
            # css
            coc-css
            # yaml
            coc-yaml
            # json
            coc-json
            # html
            coc-html;
        };
      };
    };
  };
  cocSettings = {
    "diagnostic.maxWindowHeight" = 60;
    languageserver = {
      nix = {
        command = "rnix-lsp";
        filetypes = [ "nix" ];
      };
      haskell = {
        command = "ghcide";
        args = [ "--lsp" ];
        rootPatterns = [ ".hie-bios" ];
        filetypes = [ "hs" "lhs" "haskell" ];
      };
    };
    explorer.icon.enableNerdfont = true;
    explorer.file.child.template =
      "[git | 2] [selection | clip | 1] [indent][icon | 1] [diagnosticError & 1][diagnosticWarning & 1][filename omitCenter 1][modified][readonly] [linkIcon & 1][link growRight 1 omitCenter 5][size]";
  };
in {
  home = {
    file.".config/nvim/coc-settings.json".text = builtins.toJSON cocSettings;
    packages = [ neovim ];
  };
}
