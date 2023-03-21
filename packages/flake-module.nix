{
  lib,
  inputs,
  ...
}: let
  stable-pkgs = inputs.nixos-stable.legacyPackages.x86_64-linux;
  unstable-pkgs = inputs.nixos-unstable.legacyPackages.x86_64-linux;
  inherit (unstable-pkgs.haskell.lib.compose) appendPatch overrideCabal;
  includePatterns = [
    ".hs"
    ".cabal"
    "LICENSE"
    "default.nix"
    "CHANGELOG.md"
  ];
  cleanCabalPackage = {
    name,
    source,
    extraPatterns ? [],
    overrides ? _: {},
  }: hpkgs: let
    cleanSource = lib.sourceFilesBySuffices source (includePatterns ++ extraPatterns);
  in
    lib.pipe {}
    [
      (hpkgs.callPackage source)
      (overrideCabal (
        old:
          {
            src = cleanSource;
            preConfigure = ''
              echo "Checking that default.nix is up-to-date …"
              ${lib.getExe hpkgs.cabal2nix} . > fresh-default.nix
              cp ${cleanSource}/default.nix .
              chmod u+w default.nix
              ${lib.getExe stable-pkgs.alejandra} -q fresh-default.nix default.nix
              ${stable-pkgs.diffutils}/bin/diff -w default.nix fresh-default.nix
              echo "default.nix confirmed to be up-to-date."
            '';
          }
          // overrides old
      ))
      hpkgs.buildFromCabalSdist
    ];
  haskellPackagesOverlay = final: prev:
    lib.mapAttrs (_: package: package final) myHaskellPackages
    // {
      # For nixpkgs-bot, this patch is already merged upstream and can be removed on the next release
      matrix-client =
        appendPatch (stable-pkgs.fetchpatch {
          url = "https://github.com/softwarefactory-project/matrix-client-haskell/commit/97cb1918fcdf9b0249c6c8e70c7bfc664d718022.patch";
          sha256 = "sha256-YyxgfNO5RtqpKJ9UOYPlRple0FuNmjAB1iy9vYy0HOE=";
          relative = "matrix-client";
        })
        prev.matrix-client;
      streamly-core = final.callHackageDirect {
        pkg = "streamly-core";
        ver = "0.1.0";
        sha256 = "sha256-hoSV6Q2+X5a7hFnJAArqNPjcMaCVyX9Vz4FcxeJ+jgI=";
      } {};
      streamly = hpkgs.callHackageDirect {
        pkg = "streamly";
        ver = "0.9.0";
        sha256 = "sha256-eOxVb8qQjZDo1+S7CStqYSExOg2QHWkMY+zlOYqwZak=";
      } {};
      streamly-bytestring = hpkgs.callHackageDirect {
        pkg = "streamly-bytestring";
        ver = "0.2.0";
        sha256 = "sha256-9mZiEVPtB0fU65nTcx2CX/3GoXbpZs5BXOgZyT+R2AY=";
      } {};
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
    status-script = cleanCabalPackage {
      name = "status-script";
      source = ./status-script;
      overrides = old: {
        buildDepends = builtins.attrValues {
          inherit (stable-pkgs) git khal playerctl notmuch jq tailscale;
          inherit (unstable-pkgs) nix nix-diff;
        };
      };
    };
  };
  hpkgs = unstable-pkgs.haskellPackages.override {
    overrides = haskellPackagesOverlay;
  };
  packages = selectHaskellPackages hpkgs;
in {
  flake = {
    lib = {inherit selectHaskellPackages;};
    overlays = {inherit haskellPackagesOverlay;};
  };
  perSystem = {config, ...}: {
    inherit packages;
  };
}
