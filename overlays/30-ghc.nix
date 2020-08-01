self: super:
let
  master = import super.sources.nixpkgs-master { };
  inherit (master.haskell.lib) overrideCabal unmarkBroken;
  makeHaskellScriptPackages = p: {
    inherit (p)
      aeson shh string-interpolate relude replace-megaparsec async say
      megaparsec fdo-notify these;
  };
  makeHaskellPackages = p:
    {
      inherit (p)
        brittany ormolu releaser cabal-fmt stack ghcid ghcide haskell-language-server cabal-install dhall
        aeson unordered-containers these taskwarrior pandoc hlint
        cabal2nix weeder reflex-dom password optics-th shh-extras neuron hspec-discover cabal-edit;
    } // makeHaskellScriptPackages p;
in {
  myHaskellPackages = makeHaskellPackages master.haskellPackages;
  scriptGhc = master.ghc.withPackages
    (p: builtins.attrValues (makeHaskellScriptPackages p));
  ghc = master.ghc.withHoogle (p: builtins.attrValues (makeHaskellPackages p));
}
