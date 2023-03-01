rec {
  makeHaskellScriptPackages = p: {
    inherit
      (p)
      aeson
      shh
      string-interpolate
      relude
      replace-megaparsec
      async
      say
      megaparsec
      fdo-notify
      these
      fsnotify
      witch
      ;
  };
  makeHaskellPackages = p:
    {
      inherit
        (p)
        releaser
        cabal-fmt
        stack
        ghcid
        haskell-language-server
        cabal-install
        dhall
        pandoc
        hlint
        cabal2nix
        nix-derivation
        ;
    }
    // makeHaskellScriptPackages p;
}
