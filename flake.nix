{
  description = "maralorns configuration";
  nixConfig = {
    allow-import-from-derivation = true;
  };

  inputs = {
    secrets = {
      url = "git+ssh://git@hera.m-0.eu/config-secrets";
      inputs.nixpkgs.follows = "nixos-unstable";
    };
    emanote = {
      url = "github:srid/emanote";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixos-unstable";
      };
    };
    nix-output-monitor = {
      inputs = {
        flake-compat.follows = "pre-commit-hooks/flake-compat";
        flake-utils.follows = "pre-commit-hooks/flake-utils";
        nixpkgs.follows = "nixos-unstable";
        pre-commit-hooks.follows = "pre-commit-hooks";
      };
      url = "git+ssh://git@hera.m-0.eu/nix-output-monitor?ref=main";
    };
    nixos-unstable.url = "nixpkgs/nixos-unstable";
    nixos-mailserver = {
      inputs = {
        flake-compat.follows = "pre-commit-hooks/flake-compat";
        utils.follows = "pre-commit-hooks/flake-utils";
        nixpkgs.follows = "nixos-unstable";
        nixpkgs-22_11.follows = "nixos-stable";
      };
      url = "git+https://gitlab.com/simple-nixos-mailserver/nixos-mailserver.git";
    };
    nixos-stable.url = "nixpkgs/nixos-22.11";
    flake-parts.inputs.nixpkgs-lib.follows = "nixos-unstable";
    home-manager = {
      url = "home-manager/release-22.11";
      inputs = {
        utils.follows = "pre-commit-hooks/flake-utils";
        nixpkgs.follows = "nixos-unstable";
      };
    };
    hexa-nur-packages = {
      url = "github:mweinelt/nur-packages";
      inputs.nixpkgs.follows = "nixos-unstable";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs-stable.follows = "nixos-stable";
        nixpkgs.follows = "nixos-unstable";
      };
    };
  };

  outputs = inputs @ {
    nixos-hardware,
    self,
    ...
  }:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit-hooks.flakeModule
        ./nixos/flake-parts.nix
        ./home-manager/flake-parts.nix
        ./packages/flake-parts.nix
        ./overlays/flake-parts.nix
      ];
      systems = ["x86_64-linux"];
      perSystem = {
        self',
        inputs',
        config,
        lib,
        pkgs,
        ...
      }: {
        devShells = {
          default = pkgs.mkShell {
            shellHook = config.pre-commit.installationScript;
          };
        };
        checks = {
          system-checks = pkgs.recursiveLinkFarm "all-configs" {
            nixos-configurations = lib.mapAttrs (_: config: config.config.system.build.toplevel) self.nixosConfigurations;
            home-manager-configurations = self.homeModes;
          };
        };

        pre-commit = {
          pkgs = inputs'.nixos-unstable.legacyPackages;
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
