{ pkgs, ... }:
{
  home.packages = with pkgs; [
    gnupg
    pass
    mutt
    sshuttle
    mtr
    youtubeDL
  ];
}
