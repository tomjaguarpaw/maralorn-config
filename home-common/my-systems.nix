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
    gcc
    binutils-unwrapped
    rustfmt
    carnix
  ];
}
