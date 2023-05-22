final: _prev: {
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  unstableGhc = final.unstable.ghc;
  inherit (final.unstable)
    nix # Always good for early nom problem detection.
    nil # Because old nil with new nix does not get cached.
  ;
}
