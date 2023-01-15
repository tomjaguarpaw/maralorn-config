{
  description = "maralorns configuration";

  inputs = {
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixos-unstable.follows = "nixpkgs";
    nixos-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixos-stable";
      };
    };
  };

  outputs = inputs @ {
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
      ];
      systems = ["x86_64-linux"];
      perSystem = {
        self',
        pkgs,
        config,
        ...
      }: {
        devShells.default = pkgs.mkShell {
          shellHook = config.pre-commit.installationScript;
        };
        pre-commit = {
          check.enable = true;
          settings.hooks = {
            hlint.enable = true;
            alejandra.enable = true;
            nix-linter.enable = false; # Too many false positives for now
            statix.enable = true;
            fourmolu.enable = true;
            shellcheck.enable = true;
          };
        };
      };
    };
}
