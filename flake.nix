{
  description = "maralorns configuration";
  nixConfig = {
    allow-import-from-derivation = true;
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
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
        lib,
        ...
      }: let
        packages = import ./packages {inherit pkgs;};
      in {
        devShells.default = packages.shell {
          shellHook = config.pre-commit.installationScript;
        };
        inherit (packages) packages;
        legacyPackages = {inherit (packages) haskellPackagesOverlay;};

        pre-commit = {
          check.enable = true;
          settings = {
            settings.ormolu.defaultExtensions = [
              "TypeApplications"
              "BangPatterns"
              "ImportQualifiedPost"
              "BlockArguments"
            ];
            hooks = {
              hlint.enable = true;
              alejandra.enable = true;
              nix-linter.enable = false; # Too many false positives for now
              statix.enable = true;
              fourmolu.enable = true;
              shellcheck.enable = true;
              cabal-fmt.enable = true;
              dhall-format.enable = true;
            };
          };
        };
      };
    };
}
