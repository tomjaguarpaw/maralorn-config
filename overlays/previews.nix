self: super: {
  nix-output-monitor = self.flake-inputs'.nix-output-monitor.packages.default;
  unstable = self.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = self.unstable.haskellPackages;
  unstableGhc = self.unstable.ghc;
  inherit
    (self.unstable)
    nix # Always good for early nom problem detection.
    nil # Because old nil with new nix does not get cached.
    ;
}
