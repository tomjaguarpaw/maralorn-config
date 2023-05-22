{ lib, inputs, ... }: {
  perSystem = { inputs', ... }: {
    _module.args.pkgs = inputs'.nixos-stable.legacyPackages.appendOverlays ([
      (_: _: {
        flake-inputs = inputs;
        flake-inputs' = inputs';
      })
    ] ++ (let
      overlayPath = ./.;
      candidates = lib.attrNames (builtins.readDir overlayPath);
      pathToOverlay = n: overlayPath + ("/" + n);
      isNixFile = n: builtins.match ".*\\.nix" n != null;
      isNixDir = n: builtins.pathExists (pathToOverlay n + "/default.nix");
      notDefault = n: n != "flake-module.nix";
      isOverlay = n: (isNixDir n || isNixFile n) && notDefault n;
      overlays = builtins.filter isOverlay candidates;
      importOverlay = n: import (pathToOverlay n);
    in map importOverlay overlays));
  };
}
