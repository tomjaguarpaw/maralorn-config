{ lib, inputs, ... }:
let
  stable-pkgs = inputs.nixos-stable.legacyPackages.x86_64-linux;
  unstable-pkgs = inputs.nixos-unstable.legacyPackages.x86_64-linux;
  inherit (unstable-pkgs.haskell.lib.compose) overrideCabal;
  includePatterns = [
    ".hs"
    ".cabal"
    "LICENSE"
    "default.nix"
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
      cleanSource = lib.sourceFilesBySuffices source (
        includePatterns ++ extraPatterns
      );
    in
    lib.pipe { } [
      (hpkgs.callPackage source)
      (overrideCabal (
        old:
        {
          src = cleanSource;
          preConfigure = ''
            echo "Checking that default.nix is up-to-date …"
            ${hpkgs.cabal2nix}/bin/cabal2nix . > fresh-default.nix
            cp ${cleanSource}/default.nix .
            chmod u+w default.nix
            ${lib.getExe hpkgs.nixfmt} fresh-default.nix default.nix
            ${stable-pkgs.diffutils}/bin/diff -uw default.nix fresh-default.nix
            echo "default.nix confirmed to be up-to-date."
          '';
        }
        // overrides old
      ))
      hpkgs.buildFromCabalSdist
    ];
  haskellPackagesOverlay =
    final: prev:
    lib.mapAttrs (_: package: package final) myHaskellPackages
    // {
      streamly = final.streamly_0_9_0;
      nixfmt = overrideCabal (_: { src = inputs.nixfmt; }) prev.nixfmt;
    }
  ;
  selectHaskellPackages =
    attrs:
    lib.mapAttrs (name: _: attrs.${name}) myHaskellPackages
    // {
      inherit (attrs) nixfmt;
    }
  ;
  myHaskellPackages = {
    wizards-dialog = cleanCabalPackage ./wizards-dialog { };
    rssfeeds = cleanCabalPackage ./rssfeeds { };
    kassandra = cleanCabalPackage ./kassandra/kassandra {
      overrides = _: { doHaddock = false; };
    };
    kassandra-standalone = cleanCabalPackage ./kassandra/standalone { };
    nixpkgs-bot = cleanCabalPackage ./nixpkgs-bot { };
    builders-configurator = cleanCabalPackage ./builders-configurator { };
    status-script = cleanCabalPackage ./status-script {
      overrides = _: {
        buildDepends = builtins.attrValues {
          inherit (stable-pkgs)
            git
            khal
            playerctl
            notmuch
            jq
            tailscale
            fd
            pipewire
            mako
          ;
          inherit (unstable-pkgs) nix nix-diff;
        };
      };
    };
  };
  hpkgs = unstable-pkgs.haskellPackages.override {
    overrides = haskellPackagesOverlay;
  };
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
