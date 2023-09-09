{
  home = {
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
          insert = common_keys // {
            up = [
              "normal_mode"
              "move_line_up"
            ];
            down = [
              "normal_mode"
              "move_line_down"
            ];
            left = [
              "normal_mode"
              "move_char_left"
            ];
            right = [
              "normal_mode"
              "move_char_right"
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
