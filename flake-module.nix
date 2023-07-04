{
  inputs,
  ...
}:
{
  imports = [
    inputs.pre-commit-hooks.flakeModule
    ./nixos/flake-module.nix
    ./home-manager/flake-module.nix
    ./packages/flake-module.nix
    ./overlays/flake-module.nix
  ];
  systems = [ "x86_64-linux" ];
  flake.nixFromDirs =
    let
      nixFromDir =
        dir:
        builtins.concatLists (
          builtins.attrValues (
            builtins.mapAttrs
              (
                name: path_type:
                if path_type == "regular" && builtins.match "[^_].*\\.nix" name != null then
                  [ (import "${dir}/${name}") ]
                else if path_type == "directory" then
                  nixFromDir "${dir}/${name}"
                else
                  [ ]
              )
              (builtins.readDir dir)
          )
        )
      ;
    in
    builtins.concatMap nixFromDir
  ;
  perSystem =
    {
      inputs',
      lib,
      config,
      pkgs,
      ...
    }:
    {
      devShells = {
        default = pkgs.mkShell { shellHook = config.pre-commit.installationScript; };
      };
      packages = {
        all-configs = pkgs.recursiveLinkFarm "all-configs" {
          nixos-configurations =
            lib.mapAttrs (_: config: config.config.system.build.toplevel)
              inputs.self.nixosConfigurations
          ;
          home-manager-configurations = inputs.self.homeModes;
        };
      };

      pre-commit =
        let
          generated_nix_files = [
            "packages/.*/default\\.nix"
            "hardware-configuration\\.nix"
          ];
        in
        {
          pkgs = inputs'.nixos-unstable.legacyPackages;
          check.enable = true;
          settings = {
            tools.nixfmt = lib.mkForce (lib.getBin pkgs.nixfmt);
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
              fourmolu = {
                enable = true;
              };
              shellcheck.enable = true;
              cabal-fmt.enable = true;
              dhall-format.enable = true;
            };
          };
        }
      ;
    }
  ;
}
