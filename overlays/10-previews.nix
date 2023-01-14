self: super: let
  unstable = import super.sources.nixos-unstable {};
  sources = builtins.fromJSON (builtins.readFile ../nix/sources.json);
  myFlake = name: (builtins.getFlake "git+ssh://git@hera.m-0.eu/${name}?rev=${sources.${name}.rev}&ref=main").packages.x86_64-linux;
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
  nixpkgs-bot = (myFlake "nixpkgs-bot").default;
  nix-output-monitor = (myFlake "nix-output-monitor").default;
  inherit (myFlake "mastodon_digest") mastodon_digest;
}
