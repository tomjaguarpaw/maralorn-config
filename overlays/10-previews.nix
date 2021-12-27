self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit unstable;
  inherit (unstable) cachix cabal2nix;
  nix = self.nix_2_4;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  home-assistant = unstable.home-assistant;
  nix-output-monitor = unstable.nix-output-monitor.overrideAttrs (old: {
    src = super.sources.nix-output-monitor;
    buildInputs = old.buildInputs ++ [ super.haskellPackages.streamly ];
  });
}
