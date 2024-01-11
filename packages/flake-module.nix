{ lib, inputs, ... }:
let
  stable-pkgs = inputs.nixos-stable.legacyPackages.x86_64-linux;
  unstable-pkgs = inputs.nixos-unstable.legacyPackages.x86_64-linux;
  inherit (unstable-pkgs.haskell.lib.compose) overrideCabal;
  includePatterns = [
    ".hs"
    ".cabal"
    "LICENSE"
    "CHANGELOG.md"
  ];
  cleanCabalPackage =
    source:
    {
      extraPatterns ? [ ],
      overrides ? _: { },
    }:
    hpkgs:
    let
      cleanSource = lib.sourceFilesBySuffices source (includePatterns ++ extraPatterns);
    in
    lib.pipe { } [
      (hpkgs.callPackage source)
      (overrideCabal (
        old:
        {
          src = cleanSource;
          enableSeparateBinOutput = old.isExecutable or false;
        }
        // overrides old
      ))
      hpkgs.buildFromCabalSdist
    ];
  haskellPackagesOverlay =
    final: prev:
    lib.mapAttrs (_: package: package final) myHaskellPackages
    // {
      nixfmt =
        overrideCabal
          (_: {
            src = inputs.nixfmt;
          })
          prev.nixfmt;
    };
  selectHaskellPackages =
    attrs: lib.mapAttrs (name: _: attrs.${name}) myHaskellPackages // { inherit (attrs) nixfmt; };
  myHaskellPackages = {
    wizards-dialog = cleanCabalPackage ./wizards-dialog { };
    rssfeeds = cleanCabalPackage ./rssfeeds { };
    kassandra = cleanCabalPackage ./kassandra/kassandra {
      overrides = _: {
        doHaddock = false;
      };
    };
    kassandra-standalone = cleanCabalPackage ./kassandra/standalone { };
    nixpkgs-bot = cleanCabalPackage ./nixpkgs-bot { };
    t = cleanCabalPackage ./t { };
    builders-configurator = cleanCabalPackage ./builders-configurator { };
    status-script = cleanCabalPackage ./status-script {
      overrides = _: {
        buildDepends = builtins.attrValues {
          inherit (stable-pkgs)
            gitMinimal
            systemd
            khal
            playerctl
            notmuch
            iputils
            tailscale
            pipewire
            mako
            yubikey-touch-detector
            ;
        };
      };
    };
  };
  hpkgs = unstable-pkgs.haskellPackages.override { overrides = haskellPackagesOverlay; };
  packages = selectHaskellPackages hpkgs;
in
{
  flake = {
    lib = {
      inherit selectHaskellPackages;
    };
    overlays = {
      inherit haskellPackagesOverlay;
    };
  };
  perSystem = {
    inherit packages;
  };
}
