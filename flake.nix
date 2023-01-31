{
  description = "maralorns configuration";
  nixConfig = {
    allow-import-from-derivation = true;
  };

  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
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
        inputs',
        pkgs,
        config,
        lib,
        ...
      }: let
        inherit (import ./packages {inherit pkgs;}) haskellPackagesOverlay selectHaskellPackages;
        hpkgs = pkgs.haskellPackages.override {
          overrides = haskellPackagesOverlay;
        };
      in {
        devShells.default = hpkgs.shellFor {
          packages = hpkgs: (builtins.attrValues (selectHaskellPackages hpkgs));
          shellHook = config.pre-commit.installationScript;
          buildInputs = [
            hpkgs.haskell-language-server
            pkgs.cabal-install
            inputs'.agenix.packages.default
          ];
        };
        packages = selectHaskellPackages hpkgs;
        legacyPackages = {inherit haskellPackagesOverlay;};

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
