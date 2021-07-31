{ pkgs, ... }: {
  home.packages = builtins.attrValues {
    inherit (pkgs.xorg) xev;
    inherit (pkgs.gitAndTools) hub;
    inherit (pkgs)
      meld icedtea8_web octave filezilla nix-review gparted
      grafana-devel;
  };
  home.file.".cabal/config".text = ''
    repository hackage.haskell.org
      url: http://hackage.haskell.org/

    username: maralorn
    password-command: pass org/haskell/hackage.haskell.org/maralorn
  '';
}
