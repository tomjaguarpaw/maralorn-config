{
  programs.lazygit = {
    enable = true;
    settings = {
      notARepository = "quit";
      disableStartupPopups = true;
      gui = {
        skipUnstageLineWarning = true;
        nerdFontsVersion = "3";
        showBranchCommitHash = true;
      };
      git.autoFetch = false; # No unsupervised ssh usage please.
      os = {
        edit = "hx {{filename}}";
        editAtLine = "hx {{filename}}:{{line}}";
        editAtLineAndWait = "hx {{filename}}:{{line}}";
        editInTerminal = true;
        openDirInEditor = "hx {{dir}}";
      };
      customCommands = [
        {
          key = "s";
          command = "git sync";
          context = "global";
          subprocess = true;
        }
        {
          key = "s";
          command = "git sync";
          context = "files";
          subprocess = true;
        }
      ];
    };
  };
}
