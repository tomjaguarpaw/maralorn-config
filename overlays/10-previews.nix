self: super: let
  unstable = import super.sources.nixos-unstable {};
  bot_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).nixpkgs-bot.rev;
  nom_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).nix-output-monitor.rev;
  nom = builtins.getFlake "git+ssh://git@hera.m-0.eu/nix-output-monitor?rev=${nom_commit}&ref=main";
  bot = builtins.getFlake "git+ssh://git@hera.m-0.eu/nixpkgs-bot?rev=${bot_commit}&ref=main";
in {
  inherit unstable;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit
    (unstable)
    nix
    home-assistant
    cabal2nix
    chrysalis
    emanote
    helix
    ;
  nixpkgs-bot = bot.packages.x86_64-linux.default;
  nix-output-monitor = nom.packages.x86_64-linux.default;
}
