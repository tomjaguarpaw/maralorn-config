self: super: let
  unstable = import super.sources.nixos-unstable {};
in {
  inherit unstable;
  nix = self.nix_2_4;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit (unstable) home-assistant vscode-extensions vscode mumble cachix cabal2nix;
  nix-output-monitor = unstable.haskell.lib.overrideCabal (unstable.haskellPackages.callCabal2nix "nix-output-monitor" super.sources.nix-output-monitor {}) {
    inherit (super.nix-output-monitor) postInstall;
    doCheck = false;
  };
}
