{
  programs.zsh = {
    enable = true;
    history = {
      path = "$HOME/.persist/.zsh_history";
      extended = true;
      expireDuplicatesFirst = true;
    };
    initExtra = builtins.readFile ./zshrc;
  };
}
