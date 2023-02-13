{
  lib,
  inputs,
  ...
}: let
  pkgs = inputs.nixos-unstable.legacyPackages.x86_64-linux;
  inherit (pkgs.haskell.lib.compose) appendPatch;
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
      # For nixpkgs-bot, this patch is already merged upstream and can be removed on the next release
      matrix-client =
        appendPatch (pkgs.fetchpatch {
          url = "https://github.com/softwarefactory-project/matrix-client-haskell/commit/97cb1918fcdf9b0249c6c8e70c7bfc664d718022.patch";
          sha256 = "sha256-YyxgfNO5RtqpKJ9UOYPlRple0FuNmjAB1iy9vYy0HOE=";
          relative = "matrix-client";
        })
        prev.matrix-client;
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
    builders-configurator = cleanCabalPackage {
      name = "builders-configurator";
      source = ./builders-configurator;
    };
  };
  hpkgs = pkgs.haskellPackages.override {
    overrides = haskellPackagesOverlay;
  };
  packages = selectHaskellPackages hpkgs;
in {
  flake.overlays = {
    inherit haskellPackagesOverlay;
    addMyHaskellPackages = _: _: packages;
  };
  perSystem = {config, ...}: {
    inherit packages;
    devShells.haskell = hpkgs.shellFor {
      packages = hpkgs: (builtins.attrValues (selectHaskellPackages hpkgs));
      shellHook = config.pre-commit.installationScript;
      buildInputs = [
        hpkgs.haskell-language-server
        hpkgs.cabal-install
      ];
    };
  };
}
