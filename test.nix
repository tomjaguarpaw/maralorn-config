let
  nix-pre-commit-hooks = import (import ./nix/sources.nix)."pre-commit-hooks.nix";
in
{
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      hlint.enable = true;
      nixpkgs-fmt.enable = true;
      fourmolu.enable = true;
    };
  };
}
