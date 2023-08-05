{
  withSystem,
  lib,
  inputs,
  ...
}:
{
  flake = {
    nixosConfigurations = withSystem "x86_64-linux" (
      { pkgs, ... }:
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
              inputs.nixos-mailserver.nixosModules.default
              inputs.home-manager.nixosModules.home-manager
            ];
          };
      in
      lib.genAttrs machines makeSystem
    );
  };
}
