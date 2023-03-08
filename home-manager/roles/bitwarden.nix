{pkgs, ...}: {
  programs.rbw = {
    enable = true;
    package = pkgs.rbw.override {
      withFzf = true;
      withPass = true;
    };
    settings = {
      email = "bitwarden@maralorn.de";
      base_url = "https://bitwarden.darmstadt.ccc.de";
      lock_timeout = 86400; # One day
      pinentry = "gnome3";
    };
  };
}
