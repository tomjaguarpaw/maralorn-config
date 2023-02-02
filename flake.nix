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
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs.follows = "nixos-unstable";
    flake-parts.inputs.nixpkgs-lib.follows = "nixos-unstable";
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

  outputs = inputs @ {nixos-hardware, ...}: let
    unstable = inputs.nixos-unstable.legacyPackages.x86_64-linux;
    inherit (import ./packages {pkgs = unstable;}) haskellPackagesOverlay selectHaskellPackages;
  in
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
      ];
      systems = ["x86_64-linux"];
      flake = {
        nixosConfigurations = import ./nixos/configurations.nix inputs;
        overlays.haskellPackages = haskellPackagesOverlay;
      };
      perSystem = {
        self',
        inputs',
        pkgs,
        config,
        lib,
        ...
      }: let
        hpkgs = pkgs.haskellPackages.override {
          overrides = inputs.self.overlays.haskellPackages;
        };
      in {
        devShells = {
          default = pkgs.mkShell {
            shellHook = config.pre-commit.installationScript;
          };
          haskell = hpkgs.shellFor {
            packages = hpkgs: (builtins.attrValues (selectHaskellPackages hpkgs));
            shellHook = config.pre-commit.installationScript;
            buildInputs = [
              hpkgs.haskell-language-server
              pkgs.cabal-install
            ];
          };
        };
        checks = {
          system-checks = pkgs.runCommand "system-checks" {} ''
            ${lib.concatMapStringsSep "\n" (x: "# ${x.config.system.build.toplevel}") (builtins.attrValues inputs.self.nixosConfigurations)}
            echo success > $out
          '';
        };
        packages = selectHaskellPackages hpkgs;

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
