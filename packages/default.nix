{pkgs}: let
  inherit (pkgs) lib;
  inherit (pkgs.haskell.lib.compose) unmarkBroken doJailbreak dontCheck appendPatch;
  includePatterns = [
    ".hs"
    ".cabal"
    "LICENSE"
    "CHANGELOG.md"
  ];
  cleanCabalPackage = {
    name,
    source,
    extraPatterns ? [],
  }: hpkgs:
    lib.pipe source
    [
      (src: lib.sourceFilesBySuffices src (includePatterns ++ extraPatterns))
      (src: hpkgs.callCabal2nix name src {})
      hpkgs.buildFromCabalSdist
    ];
  haskellPackagesOverlay = final: prev:
    lib.mapAttrs (_: package: package final) myHaskellPackages
    // {
      # For kassandra
      clay = unmarkBroken (doJailbreak prev.clay);
      streamly-bytestring =
        unmarkBroken (dontCheck prev.streamly-bytestring);
      # For nixpkgs-bot
      matrix-client =
        appendPatch (pkgs.fetchpatch {
          url = "https://github.com/softwarefactory-project/matrix-client-haskell/commit/97cb1918fcdf9b0249c6c8e70c7bfc664d718022.patch";
          sha256 = "sha256-YyxgfNO5RtqpKJ9UOYPlRple0FuNmjAB1iy9vYy0HOE=";
          relative = "matrix-client";
        })
        prev.matrix-client;
      aeson-schemas = unmarkBroken (dontCheck prev.aeson-schemas);
    };
  selectHaskellPackages = attrs: lib.mapAttrs (name: _: attrs.${name}) myHaskellPackages;
  myHaskellPackages = {
    wizards-dialog = cleanCabalPackage {
      name = "wizards-dialog";
      source = ./wizards-dialog;
    };
    logfeed = cleanCabalPackage {
      name = "rssfeeds";
      source = ./rssfeeds;
    };
    kassandra = cleanCabalPackage {
      name = "kassandra";
      source = ./kassandra/kassandra;
    };
    kassandra-standalone = cleanCabalPackage {
      name = "standalone";
      source = ./kassandra/standalone;
    };
    nixpkgs-bot = cleanCabalPackage {
      name = "nixpkgs-bot";
      source = ./nixpkgs-bot;
    };
  };
in {
  inherit selectHaskellPackages haskellPackagesOverlay;
}
