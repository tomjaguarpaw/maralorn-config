self: super: let
  unstable = import super.sources.nixos-unstable {};
  nom_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).nix-output-monitor.rev;
  hx_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).helix.rev;
  bot_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).nixpkgs-bot.rev;
  hx = builtins.getFlake "github:helix-editor/helix/${hx_commit}";
  nom = builtins.getFlake "git+ssh://git@hera.m-0.eu/nix-output-monitor?rev=${nom_commit}&ref=main";
  bot = builtins.getFlake "git+ssh://git@hera.m-0.eu/nixpkgs-bot?rev=${bot_commit}&ref=main";
in {
  inherit unstable;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit (unstable) nix home-assistant vscode-extensions vscodium cachix cabal2nix chrysalis nil taplo tut;
  nix-output-monitor = nom.packages.x86_64-linux.default;
  helix = hx.packages.x86_64-linux.default;
  nixpkgs-bot = bot.packages.x86_64-linux.default;
}
