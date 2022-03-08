let
  nix-pre-commit-hooks = import (import ./nix/sources.nix)."pre-commit-hooks.nix";
in {
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    excludes = ["nix/.*" "hardware-configuration.nix"];
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
