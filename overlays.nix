{ lib }:
let
  overlayPath = ./overlays;
  candidates = lib.attrNames (builtins.readDir overlayPath);
  pathToOverlay = n: overlayPath + ("/" + n);
  isNixFile = n: builtins.match ".*\\.nix" n != null;
  isNixDir = n: builtins.pathExists (pathToOverlay n + "/default.nix");
  isOverlay = n: isNixDir n || isNixFile n;
  overlays = builtins.filter isOverlay candidates;
  importOverlay = n: import (pathToOverlay n);
in map importOverlay overlays
