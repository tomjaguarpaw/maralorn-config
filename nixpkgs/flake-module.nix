{ lib, inputs, ... }:
{
  perSystem =
    { inputs', system, ... }:
    {
      _module.args.pkgs = import inputs.nixos-stable {
        inherit system;
        config = {
          permittedInsecurePackages = [
            "zotero-6.0.26" # https://nvd.nist.gov/vuln/detail/CVE-2023-5217
          ];
          allowUnfreePredicate =
            pkg:
            builtins.elem (lib.getName pkg) [
              "zoom"
              "discord"
              "steam"
              "steam-run"
              "steam-original"
              "factorio-alpha"

              # Needed for pyntitrokey
              "nrfutil"
              "pc-ble-driver-py"
              "pc-ble-driver"
              "pypemicro"
            ];
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
