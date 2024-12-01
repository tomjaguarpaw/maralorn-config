{ lib, inputs, ... }:
{
  perSystem =
    { inputs', system, ... }:
    let
      setupPkgs = inputs'.nixos-unstable.legacyPackages;
      patchedNixpkgs = setupPkgs.applyPatches {
        src = inputs.nixos-stable;
        name = "patched-nixos-stable";
        patches = [
          (setupPkgs.fetchpatch {
            url = "https://github.com/NixOS/nixpkgs/commit/df4fb1f5c98cb946e0d9d3b638bc42d10a578324.diff";
            hash = "sha256-22YoRu9+MdxDmTyxB55iDenhv45vEgAlkdGTE8nJET8=";
          })
        ];
      };
    in
    {
      _module.args.pkgs = import patchedNixpkgs {
        inherit system;
        config = {
          allowUnfreePredicate =
            pkg:
            builtins.elem (lib.getName pkg) [
              "zoom"
              "discord"
              "steam"
              "steam-run"
              "steam-original"
              "steam-unwrapped"
              "factorio-space-age"
              "teamviewer"
            ];
          permittedInsecurePackages = [ "olm-3.2.16" ];
        };
        overlays =
          [
            (_: _: {
              flake-inputs = inputs;
              flake-inputs' = inputs';
            })
          ]
          ++ (
            let
              overlayPath = ./.;
              candidates = lib.attrNames (builtins.readDir overlayPath);
              pathToOverlay = n: overlayPath + ("/" + n);
              isNixFile = n: builtins.match ".*\\.nix" n != null;
              isNixDir = n: builtins.pathExists (pathToOverlay n + "/default.nix");
              notDefault = n: n != "flake-module.nix";
              isOverlay = n: (isNixDir n || isNixFile n) && notDefault n;
              overlays = builtins.filter isOverlay candidates;
              importOverlay = n: import (pathToOverlay n);
            in
            map importOverlay overlays
          );
      };
    };
}
