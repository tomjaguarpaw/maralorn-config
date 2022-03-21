self: super: let
  unstable = import super.sources.nixos-unstable {};
in {
  inherit unstable;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit (unstable) nix home-assistant vscode-extensions vscodium mumble cachix cabal2nix alejandra;
  nix-output-monitor = super.haskell.lib.overrideCabal unstable.nix-output-monitor {
    src = super.sources.nix-output-monitor;
  };
}
