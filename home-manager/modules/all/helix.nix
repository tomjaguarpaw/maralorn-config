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
    "connect-app"
  ];
  custom_configs = {
    connect-app = [ {
      name = "haskell";
      formatter = {
        command = "ormolu";
        args = [ "--no-cabal" ];
      };
    } ];
  };
  project_configs = builtins.zipAttrsWith (_: lib.concatLists) [
    custom_configs
    (lib.genAttrs format_haskell (
      _: [ {
        name = "haskell";
        auto-format = true;
      } ]
    ))
    (lib.genAttrs format_nix (
      _: [ {
        name = "nix";
        auto-format = true;
      } ]
    ))
  ];
in
{

  home = {
    activation.configureHelixProjects = lib.concatStringsSep "\n" (
      lib.mapAttrsToList
        (name: value: ''
          if [ -d "${config.home.homeDirectory}/git/${name}" ]; then
            mkdir -p "${config.home.homeDirectory}/git/${name}/.helix"
            ln -sf ${
              (pkgs.formats.toml { }).generate "languages.toml" { language = value; }
            } ${config.home.homeDirectory}/git/${name}/.helix/languages.toml
          fi
        '')
        project_configs
    );
    sessionVariables = {
      EDITOR = "hx";
      VISUAL = "hx";
    };
  };
  programs.helix = {
    enable = true;
    themes.mytheme = {
      inherits = "catppuccin_mocha";
      "ui.background".alpha = 1.0;
      "diagnostic.info".underline = {
        color = "blue";
        style = "curl";
      };
      "diagnostic.hint".underline = {
        color = "green";
        style = "curl";
      };
      "diagnostic.warning".underline = {
        color = "peach";
        style = "curl";
      };
      "diagnostic.error".underline = {
        color = "maroon";
        style = "curl";
      };
    };
    settings = {
      theme = "mytheme";
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
    languages.language = [
      {
        name = "haskell";
        config.languageServerHaskell.formattingProvider = "fourmolu";
      }
      {
        name = "comment";
        language-server.command = "ltex-ls";
        config.ltex.additionalRules = {
          enablePickyRules = true;
          completionEnabled = true;
        };
      }
      {
        name = "markdown";
        language-server.command = "ltex-ls";
        file-types = [
          "md"
          "markdown"
          "txt"
        ];
        config.ltex.additionalRules = {
          enablePickyRules = true;
          completionEnabled = true;
        };
      }
      {
        name = "nix";
        formatter.command = "nixfmt";
      }
    ];
  };
}
