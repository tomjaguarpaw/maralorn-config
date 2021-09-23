rec {
  makeHaskellScriptPackages = p: {
    inherit (p)
      aeson shh string-interpolate relude replace-megaparsec async say
      megaparsec fdo-notify these fsnotify
      ;
  };
  makeHaskellPackages = p:
    {
      inherit (p)
        releaser cabal-fmt stack ghcid ghcide iCalendar pretty-simple
        stm-containers streamly haskell-language-server cabal-install dhall
        taskwarrior pandoc hlint cabal2nix weeder reflex-dom password optics
        shh-extras neuron hmatrix postgresql-simple nix-derivation req witch
        ;
    } // makeHaskellScriptPackages p;
}
