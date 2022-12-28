let
  commit = (builtins.fromJSON (builtins.readFile ./nix/sources.json))."pre-commit-hooks.nix".rev;
  nix-pre-commit-hooks = builtins.getFlake "github:cachix/pre-commit-hooks.nix/${commit}";
in {
  pre-commit-check = nix-pre-commit-hooks.lib.x86_64-linux.run {
    src = ./.;
    hooks = {
      hlint.enable = true;
      alejandra.enable = true;
      nix-linter.enable = false; # Too many false positives for now
      statix.enable = true;
      fourmolu.enable = true;
      shellcheck.enable = true;
    };
  };
}
