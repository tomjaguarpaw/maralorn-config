{
  inputs,
  config,
  ...
}: {
  imports = [
    inputs.pre-commit-hooks.flakeModule
    ./nixos/flake-module.nix
    ./home-manager/flake-module.nix
    ./packages/flake-module.nix
    ./overlays/flake-module.nix
  ];
  systems = ["x86_64-linux"];
  perSystem = {
    inputs',
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
        nixos-configurations = lib.mapAttrs (_: config: config.config.system.build.toplevel) config.flake.nixosConfigurations;
        home-manager-configurations = config.flake.homeModes;
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
}
