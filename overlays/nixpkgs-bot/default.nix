final: prev: let
  inherit (final) lib;
  inherit (final.haskell.lib.compose) justStaticExecutables dontCheck markUnbroken appendPatch;
  haskellPackages = final.haskellPackages.override {
    overrides = hfinal: hprev: {
      matrix-client =
        appendPatch (final.fetchpatch {
          url = "https://github.com/softwarefactory-project/matrix-client-haskell/commit/97cb1918fcdf9b0249c6c8e70c7bfc664d718022.patch";
          sha256 = "sha256-YyxgfNO5RtqpKJ9UOYPlRple0FuNmjAB1iy9vYy0HOE=";
          relative = "matrix-client";
        })
        hprev.matrix-client;
      aeson-schemas = markUnbroken (dontCheck hprev.aeson-schemas);
    };
  };
  cleanSelf = lib.sourceFilesBySuffices ../../apps/nixpkgs-bot [
    ".hs"
    ".cabal"
    "CHANGELOG.md"
    "LICENSE"
  ];
in {
  nixpkgs-bot =
    lib.pipe {}
    [
      (haskellPackages.callCabal2nix "nixpkgs-bot" cleanSelf)
      haskellPackages.buildFromCabalSdist
      justStaticExecutables
    ];
}
