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
    nixos-unstable.url = "nixpkgs/nixos-unstable";
    nixos-stable.url = "nixpkgs/nixos-22.11";
    nixpkgs.follows = "nixos-unstable";
    flake-parts.inputs.nixpkgs-lib.follows = "nixos-unstable";
    home-manager = {
      url = "home-manager/release-22.11";
      inputs = {
        utils.follows = "pre-commit-hooks-nix/flake-utils";
        nixpkgs.follows = "nixos-unstable";
      };
    };
    hexa-nur-packages = {
      url = "github:mweinelt/nur-packages";
      inputs.nixpkgs.follows = "nixos-unstable";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs-stable.follows = "nixos-stable";
        nixpkgs.follows = "nixos-unstable";
      };
    };
  };

  outputs = inputs @ {nixos-hardware, ...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        ./nixos/configurations.nix
        ./home-manager/modes.nix
        ./packages
      ];
      systems = ["x86_64-linux"];
      perSystem = {
        self',
        inputs',
        pkgs,
        config,
        lib,
        ...
      }: {
        devShells = {
          default = pkgs.mkShell {
            shellHook = config.pre-commit.installationScript;
          };
        };
        checks = {
          system-checks = pkgs.runCommand "system-checks" {} ''
            mkdir -p $out
            ${lib.concatMapStringsSep "\n" (x: x) (lib.mapAttrsToList (name: x: "ln -s ${x.config.system.build.toplevel} $out/${name}-system") inputs.self.nixosConfigurations)}
            ${lib.concatMapStringsSep "\n" (x: x) (lib.mapAttrsToList (name: x: "ln -s ${x} $out/${name}-home") inputs.self.homeModes)}
          '';
        };

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
