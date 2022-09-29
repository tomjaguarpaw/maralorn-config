self: super: let
  unstable = import super.sources.nixos-unstable {};
  nom_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).nix-output-monitor.rev;
  nom = builtins.getFlake "git+ssh://git@hera.m-0.eu/nix-output-monitor?rev=${nom_commit}&ref=main";
  chrysalis-pkgs = builtins.getFlake "github:maralorn/nixpkgs/5169d354b34311aac607dbd8c8fd84962e2b1d75";
in {
  inherit unstable;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit (unstable) nix home-assistant vscode-extensions vscodium cachix cabal2nix;
  nix-output-monitor = nom.packages.x86_64-linux.default;
  inherit (chrysalis-pkgs.legacyPackages.x86_64-linux) chrysalis;
}
