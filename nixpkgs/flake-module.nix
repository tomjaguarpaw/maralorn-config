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
            url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/358404.patch";
            hash = "sha256-Twd4sWDS8FToatRrepmAWEoeIQHO7Qxm9/yBWh0El9k=";
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
