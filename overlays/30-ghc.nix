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
        brittany ormolu releaser cabal-fmt stack ghcid ghcide arbtt cabal-edit iCalendar
        haskell-language-server cabal-install dhall taskwarrior pandoc hlint
        cabal2nix weeder reflex-dom password optics shh-extras neuron
        hspec-discover paths hmatrix postgresql-simple snap
        hedgehog nix-derivation
        ;
    } // makeHaskellScriptPackages p;
  overrides = self: super: {
    iCalendar = overrideCabal (doJailbreak (unmarkBroken super.iCalendar)) {
      preConfigure = ''substituteInPlace iCalendar.cabal --replace "network >=2.6 && <2.7" "network -any"'';
      #configureFlags = [ "--allow-newer=network" ]; # try this on ghc 9.0
    };
    arbtt = doJailbreak super.arbtt;
    cabal-edit = doJailbreak super.cabal-edit;
  };
  haskellPackages = master.haskellPackages.extend overrides;
  ghc = haskellPackages.ghc;
in
{
  inherit ghc haskellPackages;
  nix-output-monitor = master.nix-output-monitor;
  myHaskellPackages = makeHaskellPackages haskellPackages;
  myHaskellScriptPackages = makeHaskellScriptPackages haskellPackages;
  ghcWithPackages = ghc.withHoogle (p: builtins.attrValues (makeHaskellPackages p));
}
