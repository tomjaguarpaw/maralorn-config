{ lib, inputs, ... }:
let
  stable-pkgs = inputs.nixos-stable.legacyPackages.x86_64-linux;
  unstable-pkgs = inputs.nixos-unstable.legacyPackages.x86_64-linux;
  inherit (unstable-pkgs.haskell.lib.compose) overrideCabal unmarkBroken;
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
      bluefin = final.callHackageDirect {
        pkg = "bluefin";
        ver = "0.0.4.2";
        sha256 = "sha256-sGxLV9BFbEU1cVWFqbLVet+dpYHNv0M+0STJ3OB+qj4=";
      } { };
      bluefin-internal = final.callHackageDirect {
        pkg = "bluefin-internal";
        ver = "0.0.4.2";
        sha256 = "sha256-Gc8kD0+4wELEu86+nDzs+tiZpkVomvssnkwh6VhIW9A=";
      } { };
      jsaddle-warp = unmarkBroken prev.jsaddle-warp;
    };
  selectHaskellPackages = attrs: lib.mapAttrs (name: _: attrs.${name}) myHaskellPackages;
  myHaskellPackages = {
    wizards-dialog = cleanCabalPackage ./wizards-dialog { };
    rssfeeds = cleanCabalPackage ./rssfeeds { };
    nixpkgs-bot = cleanCabalPackage ./nixpkgs-bot { };
    t = cleanCabalPackage ./t { };
    cass = cleanCabalPackage ./cass {
      overrides = _: { buildDepends = builtins.attrValues { inherit (stable-pkgs) tailwindcss; }; };
    };
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
