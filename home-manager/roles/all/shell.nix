{
  programs = {
    zsh = {
      enable = true;
      historySubstringSearch = {
        enable = true;
        searchDownKey = [ "$terminfo[kcud1]" ];
        searchUpKey = [ "$terminfo[kcuu1]" ];
      };
      history = {
        path = "$HOME/.persist/.zsh_history";
        extended = true;
        expireDuplicatesFirst = true;
        save = 10000000;
        size = 10000000;
      };
    };
    direnv = {
      enable = true;
      config.global.warn_timeout = "1h";
    };
  };
}
