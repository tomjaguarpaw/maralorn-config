self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit unstable;
  inherit (unstable) cachix nix-output-monitor cabal2nix;
  nix = self.nix_2_4;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  home-assistant = unstable.home-assistant;
}
