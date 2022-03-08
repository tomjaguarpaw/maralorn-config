self: super: let
  unstable = import super.sources.nixos-unstable {};
in {
  inherit unstable;
  nix = self.nix_2_4;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit (unstable) home-assistant vscode-extensions vscode mumble cachix cabal2nix;
  nix-output-monitor = unstable.nix-output-monitor.overrideAttrs (old: {
    src = super.sources.nix-output-monitor;
    buildInputs = old.buildInputs ++ (with unstable.haskellPackages; [streamly optics generic-optics extra safe MemoTrie]);
  });
}
