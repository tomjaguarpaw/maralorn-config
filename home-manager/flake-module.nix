{
  withSystem,
  lib,
  inputs,
  ...
}:
{
  flake = withSystem "x86_64-linux" (
    { pkgs, ... }:
    let
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
      machines = import ./machines.nix inputs;
      buildHomeManager =
        config:
        (inputs.home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ config ];
        });
      buildModesForHost =
        host: modes:
        (pkgs.recursiveLinkFarm "${host}-modes" (
          lib.mapAttrs (_: config: (buildHomeManager config).activationPackage) modes
        ));
    in
    {
      homeConfigurations = lib.mapAttrs (_: buildHomeManager) (flattenAttrs machines);
      homeModes = lib.mapAttrs buildModesForHost { inherit (machines) zeus apollo; };
    }
  );
}
