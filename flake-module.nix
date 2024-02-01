{
  inputs,
  withSystem,
  lib,
  ...
}:
{
  imports = [
    inputs.pre-commit-hooks.flakeModule
    ./nixos/flake-module.nix
    ./packages/flake-module.nix
    ./nixpkgs/flake-module.nix
  ];
  systems = [ "x86_64-linux" ];
  flake = {
    mylib = {
      flattenAttrs =
        attrs:
        lib.listToAttrs (
          lib.flatten (
            lib.mapAttrsToList
              (
                outer_key:
                lib.mapAttrsToList (
                  inner_key: value: {
                    name = "${outer_key}-${inner_key}";
                    inherit value;
                  }
                )
              )
              attrs
          )
        );
      nixFromDirs =
        let
          nixFromDir =
            dir:
            builtins.concatLists (
              builtins.attrValues (
                builtins.mapAttrs
                  (
                    name: path_type:
                    if path_type == "regular" && builtins.match "[^_].*\\.nix" name != null then
                      [ (import (dir + "/${name}")) ]
                    else if path_type == "directory" then
                      nixFromDir (dir + "/${name}")
                    else
                      [ ]
                  )
                  (builtins.readDir dir)
              )
            );
        in
        builtins.concatMap nixFromDir;
    };
    iso = withSystem "x86_64-linux" (
      { pkgs, ... }:
      (pkgs.nixos {
        imports = [
          ./iso.nix
          "${inputs.nixos-stable}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-gnome.nix"
        ];
      }).config.system.build.isoImage
    );
  };

  perSystem =
    {
      inputs',
      lib,
      config,
      pkgs,
      ...
    }:
    {
      devShells.default = pkgs.mkShell { shellHook = config.pre-commit.installationScript; };

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
            excludes = [ "\\.zsh$" ];
            settings = {
              ormolu.defaultExtensions = [
                "TypeApplications"
                "BangPatterns"
                "ImportQualifiedPost"
                "BlockArguments"
              ];
              statix.ignore = generated_nix_files;
            };
            hooks = {
              hlint.enable = true;
              nixfmt = {
                enable = true;
                excludes = generated_nix_files;
              };
              cabal2nix.enable = true;
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
            };
          };
        };
    };
}
