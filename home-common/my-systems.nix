{ pkgs, ... }:
{
  home.packages = with pkgs; [
    gnupg
    pass
    mutt
    sshuttle
    mtr
    youtubeDL

    cargo
    rustfmt
  ];
}
