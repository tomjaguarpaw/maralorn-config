{
  programs.lazygit = {
    enable = true;
    settings = {
      gui.skipUnstageLineWarning = true;
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
