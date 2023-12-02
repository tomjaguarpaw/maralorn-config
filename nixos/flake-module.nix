{
  withSystem,
  lib,
  inputs,
  ...
}:
let
  machines = builtins.attrNames (builtins.readDir ./machines);
in
{
  flake.nixosConfigurations = withSystem "x86_64-linux" (
    {pkgs, system, ...}:
    lib.genAttrs machines (
      name:
      inputs.nixos-stable.lib.nixosSystem {
        inherit system;
        modules = [
          {
            config.nixpkgs = {
              inherit pkgs;
            };
          }
          (import (./. + "/machines/${name}/configuration.nix") inputs)
          inputs.secrets.nixosModules.default
          inputs.impermanence.nixosModules.impermanence
          inputs.disko.nixosModules.disko
          inputs.nixos-mailserver.nixosModules.default
          inputs.home-manager.nixosModules.home-manager
        ];
        specialArgs = {
          inherit (inputs.self) mylib;
        };
      }
    )
  );
}
