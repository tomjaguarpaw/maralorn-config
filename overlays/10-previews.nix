self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit unstable;
  inherit (unstable) cachix nix-output-monitor cabal2nix;
  nix = unstable.nix;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  mautrix-signal = unstable.mautrix-signal;
  signald = unstable.signald;
}
