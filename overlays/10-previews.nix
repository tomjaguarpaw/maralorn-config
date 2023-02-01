self: super: let
  sources = builtins.fromJSON (builtins.readFile ../nix/sources.json);
  myFlake = name: (builtins.getFlake "git+ssh://git@hera.m-0.eu/${name}?rev=${sources.${name}.rev}&ref=main").packages.x86_64-linux;
in {
  unstableHaskellPackages = self.unstable.haskellPackages;
  unstableGhc = self.unstable.ghc;
  inherit
    (self.unstable)
    nix
    home-assistant
    cabal2nix
    chrysalis
    emanote
    helix
    ;
  nix-output-monitor = (myFlake "nix-output-monitor").default;
}
