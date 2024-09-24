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
    {
      github = unstable-pkgs.haskell.lib.compose.appendPatch (unstable-pkgs.fetchpatch {
        url = "https://github.com/magthe/haskell-github/commit/3809dc7f56724e7ea336daeb67caf86f6c3efdb6.patch";
        hash = "sha256-JH2deKDdmSR6r75FlEp9EHzn3RG/3R8zl/IHSmmLPZA=";
      }) prev.github;
    }
    // lib.mapAttrs (_: package: package final) myHaskellPackages;
  selectHaskellPackages = attrs: lib.mapAttrs (name: _: attrs.${name}) myHaskellPackages;
  myHaskellPackages = {
    wizards-dialog = cleanCabalPackage ./wizards-dialog { };
    merge-bot = cleanCabalPackage ./merge-bot { };
    rssfeeds = cleanCabalPackage ./rssfeeds { };
    nixpkgs-bot = cleanCabalPackage ./nixpkgs-bot { };
    t = cleanCabalPackage ./t { };
    kass = cleanCabalPackage ./kass {
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
