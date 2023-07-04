{
  withSystem,
  lib,
  inputs,
  ...
}:
{
  flake = {
    nixosConfigurations = withSystem "x86_64-linux" (
      {
        pkgs,
        ...
      }:
      let
        machines = builtins.attrNames (builtins.readDir ./machines);
        makeSystem =
          name:
          pkgs.nixos {
            imports = [
              (import (./. + "/machines/${name}/configuration.nix") inputs)
              inputs.secrets.nixosModules.default
              inputs.impermanence.nixosModules.impermanence
              inputs.disko.nixosModules.disko
            ];
          }
        ;
      in
      lib.genAttrs machines makeSystem
    );
  };
}
