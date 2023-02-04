self: super: {
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
