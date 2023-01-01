{
  pkgs,
  config,
  ...
}: let
  language-servers = {
    inherit
      (pkgs.nodePackages)
      typescript-language-server
      vscode-json-languageserver-bin
      vscode-html-languageserver-bin
      vscode-css-languageserver-bin
      ;
    inherit (pkgs.python3Packages) python-lsp-server;
    inherit
      (pkgs)
      rust-analyzer
      taplo
      # toml
      
      nil
      # nix
      
      texlab
      # latex
      
      lean
      yaml-language-server
      ltex-ls
      ;
    # languagetool support for markdown
  };
in {
  home = {
    packages = builtins.attrValues language-servers;
    sessionVariables = {
      EDITOR = "hx";
      VISUAL = "hx";
    };
  };
  programs.helix = {
    enable = true;
    themes.mytheme = {
      inherits = "catppuccin_latte";
      "diagnostic.info".underline = {
        color = "blue";
        style = "curl";
      };
      "diagnostic.hint".underline = {
        color = "green";
        style = "curl";
      };
      "diagnostic.warning".underline = {
        color = "yellow";
        style = "curl";
      };
      "diagnostic.error".underline = {
        color = "red";
        style = "curl";
      };
    };
    settings = {
      theme = "mytheme";
      keys = let
        common_keys = {
          "C-s" = ":w";
          "C-f" = ":format";
          "C-r" = ":reflow";
        };
      in {
        normal = common_keys;
        insert =
          common_keys
          // {
            up = ["normal_mode" "move_line_up"];
            down = ["normal_mode" "move_line_down"];
            left = ["normal_mode" "move_char_left"];
            right = ["normal_mode" "move_char_right"];
          };
      };
      editor = {
        whitespace.render = {
          space = "all";
          tab = "all";
        };
        lsp.display-messages = true;
        indent-guides.render = true;
        cursorline = true;
        cursor-shape = {
          insert = "bar";
          normal = "underline";
        };
        file-picker.git-ignore = false;
        color-modes = true;
        bufferline = "multiple";
        auto-save = true;
      };
    };
    languages = [
      {
        name = "haskell";
        config.languageServerHaskell.formattingProvider = "fourmolu";
      }
      {
        name = "comment";
        language-server.command = "ltex-ls";
        config.ltex.additionalRules = {
          enablePickyRules = true;
          motherTongue = "de-DE";
          completionEnabled = true;
        };
      }
      {
        name = "markdown";
        language-server.command = "ltex-ls";
        file-types = ["md" "markdown" "txt"];
        config.ltex.additionalRules = {
          enablePickyRules = true;
          motherTongue = "de-DE";
          completionEnabled = true;
        };
      }
      {
        name = "nix";
        formatter = {
          command = "alejandra";
          args = ["-q"];
        };
      }
    ];
  };
}
