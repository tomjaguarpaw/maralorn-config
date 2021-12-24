{ pkgs, config, ... }:
let
  list = builtins.attrValues;
  cocSettings = {
    "diagnostic.maxWindowHeight" = 60;
    "diagnostic.virtualText" = true;
    "diagnostic.virtualTextCurrentLineOnly" = false;
    "codeLens.enable" = true;
    languageserver = {
      nix = {
        command = "rnix-lsp";
        filetypes = [ "nix" ];
      };
      haskell = {
        command = "haskell-language-server";
        args = [ "--lsp" "-d" "-l" "/tmp/LanguageServer.log" ];
        rootPatterns = [ ".hie-bios" "cabal.project" ];
        filetypes = [ "hs" "lhs" "haskell" ];
        settings.languageServerHaskell.formattingProvider = "fourmolu";
      };
    };
    explorer.icon.enableNerdfont = true;
    explorer.file.child.template =
      "[git | 2] [selection | clip | 1] [indent][icon | 1] [diagnosticError & 1][diagnosticWarning & 1][filename omitCenter 1][modified][readonly] [linkIcon & 1][link growRight 1 omitCenter 5][size]";
  };
in
{
  imports = [ ./spelling.nix ];
  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = builtins.readFile ./vimrc;
    plugins = list {
      inherit (pkgs.vimPlugins)
        # coc-tabnine (TODO: Why doesn‘t it work?)
        # TODO: tabnine config in home-manager
        # TODO: tabnine lsp: nix, rust, pandoc/latex lsp? was noch?

        # ===
        # Basic IDE plugins
        coc-nvim airline
        # same word highlighting when not supported by language
        coc-highlight coc-explorer
        # searches
        coc-fzf fzf-vim

        # general whitespace
        vim-trailing-whitespace vim-autoformat

        # Git
        coc-git# statusline, numberline and explorer infos
        fugitive# various git commands

        # Commenting and Uncommenting
        nerdcommenter

        # Theme
        papercolor-theme vim-airline-themes
        LanguageTool-nvim

        # ===
        # Languages
        # haskell syntax highlighting
        haskell-vim vim-hoogle
        # nix syntax highlighting
        vim-nix vim-markdown
        # latex
        vimtex coc-vimtex# not sure if I need two
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
        coc-html
        # dhall
        dhall-vim
        ;
    };
  };
  xdg.configFile."nvim/coc-settings.json".text = builtins.toJSON cocSettings;
  home = {
    packages = [ pkgs.languagetool ];
    sessionVariables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}
