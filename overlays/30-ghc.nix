self: super:
let
  inherit (super) fetchFromGitHub;
  master = import super.sources.nixpkgs-master {};
  inherit (master.haskell.lib) overrideCabal unmarkBroken doJailbreak;
  makeHaskellScriptPackages = p: {
    inherit (p)
      aeson shh string-interpolate relude replace-megaparsec async say
      megaparsec fdo-notify these fsnotify
      ;
  };
  makeHaskellPackages = p:
    {
      inherit (p)
        brittany ormolu releaser cabal-fmt stack ghcid ghcide
        haskell-language-server cabal-install dhall taskwarrior pandoc hlint
        cabal2nix weeder reflex-dom password optics shh-extras neuron
        hspec-discover cabal-edit paths hmatrix postgresql-simple snap
        arbtt hedgehog nix-derivation
        ;
      iCalendar = overrideCabal (doJailbreak (unmarkBroken p.iCalendar)) {
        preConfigure = ''substituteInPlace iCalendar.cabal --replace "network >=2.6 && <2.7" "network -any"'';
      };
    } // makeHaskellScriptPackages p;
  inherit (master) ghc haskellPackages;
in
{
  inherit ghc haskellPackages;
  nix-output-monitor = master.nix-output-monitor;
  myHaskellPackages = makeHaskellPackages haskellPackages;
  myHaskellScriptPackages = makeHaskellScriptPackages haskellPackages;
  ghcWithPackages = ghc.withHoogle (p: builtins.attrValues (makeHaskellPackages p));
}
