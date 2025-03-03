{ lib, inputs, ... }:
let
  stable-pkgs = inputs.nixos-stable.legacyPackages.x86_64-linux;
  unstable-pkgs = inputs.nixos-unstable.legacyPackages.x86_64-linux;
  inherit (unstable-pkgs.haskell.lib.compose) overrideCabal disableLibraryProfiling;
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
      disableLibraryProfiling
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
    {
      github =
        unstable-pkgs.haskell.lib.compose.appendPatch

          (unstable-pkgs.fetchpatch {
            url = "https://github.com/haskell-github/github/commit/bc3cf9ff81695c07d3be3bd424ddc6fb22c1ec87.patch";
            hash = "sha256-raRErVkXuuIWmY15OZUVen9Xmd+EhxXilTWOdgmn24U=";
          })
          prev.github;
    }
    // lib.mapAttrs (_: package: package final) myHaskellPackages;
  selectHaskellPackages = attrs: lib.mapAttrs (name: _: attrs.${name}) myHaskellPackages;
  myHaskellPackages = {
    wizards-dialog = cleanCabalPackage ./wizards-dialog { };
    merge-bot = cleanCabalPackage ./merge-bot { };
    rssfeeds = cleanCabalPackage ./rssfeeds { };
    nixpkgs-bot = cleanCabalPackage ./nixpkgs-bot { };
    vikunja-tools = cleanCabalPackage ./vikunja-tools {
      overrides = _: { buildDepends = builtins.attrValues { inherit (stable-pkgs) mblaze; }; };
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
            hyprland
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
