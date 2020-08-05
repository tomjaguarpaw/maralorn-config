{ pkgs, ... }: {
  home.packages = builtins.attrValues {
    inherit (pkgs.xorg) xev;
    inherit (pkgs.gitAndTools) hub;
    inherit (pkgs)
      meld icedtea8_web octave filezilla nix-review gparted
      grafana-devel;
  };
  systemd.user.services.hoogle = {
    Unit.Description = "Hoogle server";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${pkgs.ghc}/bin/hoogle server --local --links";
      Restart = "always";
    };
  };
}
