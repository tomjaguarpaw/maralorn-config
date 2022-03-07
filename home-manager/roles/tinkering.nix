{ pkgs, ... }: {
  home.packages = builtins.attrValues {
    inherit (pkgs.xorg) xev;
    inherit (pkgs)
      meld icedtea8_web octave filezilla nix-review gparted
      grafana-devel;
  };
  home.file = {
    ".config/VSCodium/User/tasks.json".text = builtins.toJSON {
      # See https://go.microsoft.com/fwlink/?LinkId=733558
      # for the documentation about the tasks.json format
      "version" = "2.0.0";
      "tasks" = [
        {
          "label" = "lazygit";
          "problemMatcher" = [ ];
          "type" = "shell";
          "command" = "lazygit";
        }
      ];
    };

    ".cabal/config".text = ''
      repository hackage.haskell.org
        url: http://hackage.haskell.org/

      username: maralorn
      password-command: pass org/haskell/hackage.haskell.org/maralorn
    '';
  };
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      haskell.haskell
      bbenoist.nix
      vscodevim.vim
      svsool.markdown-memo
      tabnine.tabnine-vscode
      gruntfuggly.todo-tree
      arrterian.nix-env-selector
      jnoortheen.nix-ide
      pkief.material-icon-theme
      yzhang.markdown-all-in-one
      justusadam.language-haskell
      naumovs.color-highlight
      timonwong.shellcheck
      usernamehw.errorlens
      valentjn.vscode-ltex
      xyz.local-history
      alefragnani.project-manager
    ];
    userSettings = {
      "editor.fontFamily" = "JetBrainsMono Nerd Font, monospace";
      "files.autoSave" = "afterDelay";
      "editor.tabSize" = 3;
      "editor.fontLigatures" = true;
      "workbench.colorTheme" = "Default Light+";
      "workbench.iconTheme" = "material-icon-theme";
      "update.mode" = "none";
      "update.showReleaseNotes" = false;
      "extensions.autoUpdate" = false;
      "extensions.autoCheckUpdates" = false;
      "local-history.path" = "~/.volatile/vscode-local-history/";
      "projectManager.git.baseFolders" = [ "~/git/" ];
      "ltex.enabled" = false;
      "memo.links.rules" = [
        {
          "rule" = "\\.md$";
          "comment" = "All notes";
          "folder" = "$CURRENT_FILE_DIRECTORY";
        }
      ];
      "vim.useCtrlKeys" = false;
      "tabnine.experimentalAutoImports" = true;
      "haskell.formattingProvider" = "fourmolu";
      "haskell.updateBehavior" = "never-check";
    };
  };
}
