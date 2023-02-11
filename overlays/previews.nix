self: super: {
  nix-output-monitor = self.flake-inputs'.nix-output-monitor.packages.default;
  unstable = self.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = self.unstable.haskellPackages;
  unstableGhc = self.unstable.ghc;
  inherit
    (self.unstable)
    nix
    home-assistant
    cabal2nix
    chrysalis
    emanote
    helix
    ;
}
