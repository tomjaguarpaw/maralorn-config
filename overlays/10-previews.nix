self: super: let
  unstable = import super.sources.nixos-unstable {};
in {
  inherit unstable;
  nix = self.nix_2_4;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit (unstable) home-assistant vscode-extensions vscode mumble cachix cabal2nix alejandra;
  nix-output-monitor = super.haskell.lib.overrideCabal unstable.nix-output-monitor {
    src = super.sources.nix-output-monitor;
  };
}
