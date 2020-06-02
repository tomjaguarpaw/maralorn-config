self: super:
let master = import super.sources.nixpkgs-master { };
in {
  myHaskellPackages = {
    inherit (master.haskellPackages)
      brittany ormolu releaser cabal-fmt stack ghcide cabal-install dhall aeson
      unordered-containers shh string-interpolate relude replace-megaparsec
      async say cmdargs megaparsec fdo-notify these neuron taskwarrior pandoc
      hlint;
  };
  ghc = master.ghc.withPackages (_: builtins.attrValues self.myHaskellPackages);
}
