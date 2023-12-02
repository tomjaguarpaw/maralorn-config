{pkgs, ...}:
{
  systemd.user.services.hoogle = {
    Unit.Description = "Hoogle server";
    Install.WantedBy = ["graphical-session.target"];
    Service = {
      ExecStart = "${pkgs.ghcWithPackages}/bin/hoogle server --local --links";
      Restart = "always";
    };
  };
  home.packages = builtins.attrValues {
    inherit (pkgs)
      ghcWithPackages
      stack
      ghcid
      cabal-install
      cabal2nix
      pandoc
      hlint
      hledger
      hledger-ui
      hledger-web
      ;
    inherit (pkgs.unstableHaskellPackages)
      haskell-language-server
      ghc-debug-client
      eventlog2html
      fourmolu
      ghc-debug-brick
      calligraphy
      cabal-fmt
      threadscope
      nix-derivation
      ;
  };
}
