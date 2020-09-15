self: super:
let
  inherit (super) fetchFromGitHub;
  master = import super.sources.nixpkgs-master { };
  inherit (master.haskell.lib) overrideCabal unmarkBroken;
  myOverrides = self: super: {
    optics = super.optics_0_3;
    optics-th = super.optics-th_0_3_0_2;
    optics-core = super.optics-core_0_3_0_1;
    optics-extra = super.optics-extra_0_3;
  };
  makeHaskellScriptPackages = p: {
    inherit (p)
      aeson shh string-interpolate relude replace-megaparsec async say
      megaparsec fdo-notify these;
  };
  makeHaskellPackages = p:
    {
      inherit (p)
        brittany ormolu releaser cabal-fmt stack ghcid ghcide haskell-language-server cabal-install dhall
        taskwarrior pandoc_2_10_1 hlint
        cabal2nix weeder reflex-dom password optics shh-extras neuron hspec-discover cabal-edit paths;
    } // makeHaskellScriptPackages p;
in {
  myHaskellPackages = makeHaskellPackages master.haskellPackages;
  scriptGhc = master.ghc.withPackages
    (p: builtins.attrValues (makeHaskellScriptPackages p));
  ghc = (master.haskellPackages.override { overrides = myOverrides; }).ghc.withHoogle (p: builtins.attrValues (makeHaskellPackages p));
}
