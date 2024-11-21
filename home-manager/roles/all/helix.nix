{
  lib,
  pkgs,
  config,
  ...
}:
let
  format_nix = [
    "nix-output-monitor"
    "config"
  ];
  format_haskell = [
    "nix-output-monitor"
    "config"
  ];
  project_configs = builtins.zipAttrsWith (_: lib.concatLists) [
    (lib.genAttrs format_haskell (_: [
      {
        name = "haskell";
        auto-format = true;
      }
    ]))
    (lib.genAttrs format_nix (_: [
      {
        name = "nix";
        auto-format = true;
      }
    ]))
  ];
in
{

  home = {
    activation.configureHelixProjects = lib.concatStringsSep "\n" (
      lib.mapAttrsToList (name: value: ''
        if [ -d "${config.home.homeDirectory}/git/${name}" ]; then
          mkdir -p "${config.home.homeDirectory}/git/${name}/.helix"
          ln -sf ${
            (pkgs.formats.toml { }).generate "languages.toml" { language = value; }
          } ${config.home.homeDirectory}/git/${name}/.helix/languages.toml
        fi
      '') project_configs
    );
    sessionVariables = {
      EDITOR = "hx";
      PAGER = "less -FRX";
      VISUAL = "hx";
    };
  };
  programs.helix = {
    enable = true;
    settings = {
      keys =
        let
          common_keys = {
            "C-s" = ":w";
            "C-f" = ":format";
          };
        in
        {
          normal = common_keys // {
            "C-r" = [
              "extend_to_line_bounds"
              ":reflow"
            ];
            " ".e = [
              "goto_next_diag"
              ":yank-diagnostic"
              ":vsplit-new"
              "paste_clipboard_after"
            ];
          };
          select = {
            "C-r" = [
              "extend_to_line_bounds"
              "join_selections"
              "keep_primary_selection"
              "extend_to_line_bounds"
              ":reflow"
            ];
          };
        };
      editor = {
        soft-wrap.enable = true;
        inline-diagnostics.cursor-line = "hint";
        end-of-line-diagnostics = "hint";
        whitespace.render = {
          space = "all";
          tab = "all";
        };
        rulers = [
          80
          100
          120
        ];
        lsp.display-messages = true;
        indent-guides.render = true;
        cursorline = true;
        cursorcolumn = true;
        auto-pairs = false;
        cursor-shape = {
          insert = "bar";
          normal = "underline";
        };
        color-modes = true;
        bufferline = "multiple";
        auto-save = true;
      };
    };
    languages = {
      language-server.haskell-language-server.config.formattingProvider = "fourmolu";
      language = [
        {
          name = "nix";
          formatter.command = "nixfmt";
        }
      ];
    };
  };
}
