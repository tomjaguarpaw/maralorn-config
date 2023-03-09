{
  pkgs,
  lib,
  ...
}: let
  package = pkgs.rbw.override {
    withFzf = true;
    withPass = true;
  };
  rbw-totp-fzf = pkgs.runCommand "rbw-totp-fzf" {} ''
    mkdir -p $out/bin
    cp ${package}/bin/rbw-fzf $out/bin/rbw-totp-fzf
    ${lib.getExe pkgs.sd} "rbw get" "rbw code" $out/bin/rbw-totp-fzf
  '';
in {
  home.packages = [rbw-totp-fzf];
  programs.rbw = {
    enable = true;
    inherit package;
    settings = {
      email = "bitwarden@maralorn.de";
      base_url = "https://bitwarden.darmstadt.ccc.de";
      lock_timeout = 86400; # One day
      pinentry = "gnome3";
    };
  };
}
