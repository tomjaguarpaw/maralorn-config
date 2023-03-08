_: {
  programs.rbw = {
    enable = true;
    settings = {
      email = "bitwarden@maralorn.de";
      base_url = "https://bitwarden.darmstadt.ccc.de";
      lock_timeout = 86400; # One day
      pinentry = "gnome3";
    };
  };
}
