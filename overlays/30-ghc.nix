final: prev:
let
  inherit (prev.haskell.lib) overrideCabal unmarkBroken doJailbreak dontCheck;
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
        pretty-simple tz stm-containers streamly haskell-language-server
        cabal-install dhall taskwarrior pandoc hlint cabal2nix weeder
        reflex-dom password optics shh-extras neuron hspec-discover paths
        hmatrix postgresql-simple snap hedgehog nix-derivation req
        witch
        ;
    } // makeHaskellScriptPackages p;
in
{
  myHaskellPackages = makeHaskellPackages prev.unstableHaskellPackages;
  myHaskellScriptPackages = makeHaskellScriptPackages prev.haskellPackages;
  ghcWithPackages = prev.unstableGhc.withHoogle (p: builtins.attrValues (makeHaskellPackages p));
}
