self: super: let
  unstable = import super.sources.nixos-unstable {};
  nom_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).nix-output-monitor.rev;
  hx_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).helix.rev;
  hx = builtins.getFlake "github:helix-editor/helix/${hx_commit}";
  nom = builtins.getFlake "git+ssh://git@hera.m-0.eu/nix-output-monitor?rev=${nom_commit}&ref=main";
in {
  inherit unstable;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit (unstable) nix home-assistant vscode-extensions vscodium cachix cabal2nix chrysalis;
  nix-output-monitor = nom.packages.x86_64-linux.default;
  helix = hx.packages.x86_64-linux.default;
}
