self: super:
let master = import super.sources.nixpkgs-master { };
in {
  myHaskellPackages = {
    inherit (master.haskellPackages)
      brittany ormolu releaser cabal-fmt stack ghcide cabal-install dhall aeson
      containers unordered-containers shh string-interpolate relude
      replace-megaparsec async say cmdargs text megaparsec fdo-notify these
      neuron taskwarrior pandoc;
  };
  ghc = master.ghc.withPackages (_: builtins.attrValues self.myHaskellPackages);
}
