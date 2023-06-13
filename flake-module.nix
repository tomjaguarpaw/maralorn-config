{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule
    ./nixos/flake-module.nix
    ./home-manager/flake-module.nix
    ./packages/flake-module.nix
    ./overlays/flake-module.nix
  ];
  systems = [ "x86_64-linux" ];
  perSystem = { inputs', lib, config, pkgs, ... }: {
    devShells = {
      default =
        pkgs.mkShell { shellHook = config.pre-commit.installationScript; };
    };
    checks = {
      system-checks = pkgs.recursiveLinkFarm "all-configs" {
        nixos-configurations =
          lib.mapAttrs (_: config: config.config.system.build.toplevel)
          inputs.self.nixosConfigurations;
        home-manager-configurations = inputs.self.homeModes;
      };
    };

    pre-commit = let
      generated_nix_files =
        [ "packages/.*/default\\.nix" "hardware-configuration\\.nix" ];
    in rec {
      pkgs = inputs'.nixos-unstable.legacyPackages;
      check.enable = true;
      settings = {
        tools.fourmolu = lib.mkForce pkgs.haskellPackages.fourmolu;
        settings.ormolu.defaultExtensions = [
          "TypeApplications"
          "BangPatterns"
          "ImportQualifiedPost"
          "BlockArguments"
        ];
        hooks = {
          hlint.enable = true;
          nixfmt.enable = true;
          nil = {
            enable = true;
            excludes = generated_nix_files;
          };
          editorconfig-checker = {
            excludes = [ ".*\\.json" ];
            enable = true;
          };
          deadnix = {
            enable = true;
            excludes = generated_nix_files;
          };
          statix.enable = true;
          fourmolu = { enable = true; };
          shellcheck.enable = true;
          cabal-fmt.enable = true;
          dhall-format.enable = true;
        };
      };
    };
  };
}
