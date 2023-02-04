{
  withSystem,
  lib,
  inputs,
  ...
}: {
  flake = withSystem "x86_64-linux" ({inputs', ...}: let
    pkgs = inputs'.nixos-stable.legacyPackages;
    flattenAttrs = attrs:
      lib.listToAttrs (lib.flatten (lib.mapAttrsToList
        (
          outer_key:
            lib.mapAttrsToList
            (inner_key: value: {
              name = "${outer_key}-${inner_key}";
              inherit value;
            })
        )
        attrs));
    machines = import ./machines.nix;
    buildHomeManager = config: (inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        config
        inputs.self.nixosModules.insertOverlays
        inputs.emanote.homeManagerModule
      ];
    });
    buildModesForHost = host: modes:
      pkgs.runCommandLocal "${host}-modes" {} ''
        mkdir $out
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (mode: config: "ln -s ${(buildHomeManager config).activationPackage} $out/${mode}") modes)}'';
  in {
    homeConfigurations = lib.mapAttrs (_: buildHomeManager) (flattenAttrs machines);
    homeModes = lib.mapAttrs buildModesForHost machines;
  });
}
