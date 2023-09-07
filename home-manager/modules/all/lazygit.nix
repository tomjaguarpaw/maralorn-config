{
  programs.lazygit = {
    enable = true;
    settings = {
      gui.skipUnstageLineWarning = true;
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
