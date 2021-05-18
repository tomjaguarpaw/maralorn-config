self: super:
let
  inherit (super) fetchFromGitHub;
  master = import super.sources.nixpkgs-master {};
  inherit (master.haskell.lib) overrideCabal unmarkBroken doJailbreak dontCheck;
  makeHaskellScriptPackages = p: {
    inherit (p)
      aeson shh string-interpolate relude replace-megaparsec async say
      megaparsec fdo-notify these fsnotify
      ;
  };
  makeHaskellPackages = p:
    {
      inherit (p)
        brittany ormolu releaser cabal-fmt stack ghcid ghcide arbtt iCalendar
        pretty-simple tz stm-containers streamly
        haskell-language-server cabal-install dhall taskwarrior pandoc hlint
        cabal2nix weeder reflex-dom password optics shh-extras neuron
        hspec-discover paths hmatrix postgresql-simple snap
        hedgehog nix-derivation req
        ;
    } // makeHaskellScriptPackages p;
    overrides = self: super: {
      generic-optics = dontCheck (unmarkBroken super.generic-optics);
    };
  haskellPackages = master.haskellPackages.extend overrides;
  ghc = haskellPackages.ghc;
in
{
  inherit ghc haskellPackages;
  cachix = master.cachix;
  nix-output-monitor = master.nix-output-monitor;
  myHaskellPackages = makeHaskellPackages haskellPackages;
  myHaskellScriptPackages = makeHaskellScriptPackages haskellPackages;
  ghcWithPackages = ghc.withHoogle (p: builtins.attrValues (makeHaskellPackages p));
}
