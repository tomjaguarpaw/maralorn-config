let
  sources = import ./nix/sources.nix;
  nix-pre-commit-hooks = import "${sources."pre-commit-hooks.nix"}/nix" {nixpkgs = sources.nixos-unstable;};
in {
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      hlint.enable = true;
      alejandra.enable = true;
      nix-linter.enable = false; # Too many false positives for now
      statix.enable = true;
      fourmolu.enable = true;
      cabal-fmt.enable = true;
      shellcheck.enable = true;
    };
  };
}
