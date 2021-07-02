self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit (unstable) cachix nix-output-monitor cabal2nix;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
}
