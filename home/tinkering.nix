{ pkgs, ... }: {
  home.packages = builtins.attrValues {
    inherit (pkgs.xorg) xev;
    inherit (pkgs.gitAndTools) hub;
    inherit (pkgs)
      meld icedtea8_web octave filezilla cachix nix-review gparted
      grafana-devel;
  };
}
