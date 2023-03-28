self: super: {
  nix-output-monitor = self.flake-inputs'.nix-output-monitor.packages.default;
  unstable = self.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = self.unstable.haskellPackages;
  unstableGhc = self.unstable.ghc;
  inherit
    (self.unstable)
    nix # Always good for early nom problem detection.
    nil # Because old nil with new nix does not get cached.
    home-assistant # hexa says that’s better than stable
    emanote # Haskell
    cabal2nix # Haskell
    chrysalis # Only until 23.05
    helix # until 23.05: Various features
    headscale # until 23.05: Required for correct hostnames and Android compat
    lklWithFirewall # until 23.05: To Fix firewall checks
    nix-diff
    musescore # version 4.0 for 22.11
    ;
}
