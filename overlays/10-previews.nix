self: super: let
  unstable = import super.sources.nixos-unstable {};
  bot_commit = (builtins.fromJSON (builtins.readFile ../nix/sources.json)).nixpkgs-bot.rev;
  bot = builtins.getFlake "git+ssh://git@hera.m-0.eu/nixpkgs-bot?rev=${bot_commit}&ref=main";
in {
  inherit unstable;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  inherit
    (unstable)
    nix
    home-assistant
    vscode-extensions
    vscodium
    cachix
    cabal2nix
    chrysalis
    nil
    taplo
    tut
    emanote
    nix-output-monitor
    helix
    ;
  nixpkgs-bot = bot.packages.x86_64-linux.default;
}
