{ withSystem, lib, inputs, ... }: {
  flake = withSystem "x86_64-linux" ({ pkgs, ... }:
    let
      flattenAttrs = attrs:
        lib.listToAttrs (lib.flatten (lib.mapAttrsToList (outer_key:
          lib.mapAttrsToList (inner_key: value: {
            name = "${outer_key}-${inner_key}";
            inherit value;
          })) attrs));
      machines = import ./machines.nix;
      buildHomeManager = config:
        (inputs.home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            config
            inputs.nix-index-database.hmModules.nix-index
            # Emanote has a ton of dependencies which I don’t want to pull
            # into this flake, so instead of depending on the flake I just load the one file
            # I need.
            (builtins.fetchurl {
              url =
                "https://raw.githubusercontent.com/srid/emanote/4a70855684099939bd8adbddec5c76b092f06643/nix/home-manager-module.nix";
              sha256 =
                "sha256:10llmlngq85hyj3gga0dx0aws4nairbcad373bm1sr7gnb65krik";
            })
          ];
        });
      buildModesForHost = host: modes:
        (pkgs.recursiveLinkFarm "${host}-modes"
          (lib.mapAttrs (_: config: (buildHomeManager config).activationPackage)
            modes)).overrideAttrs (old: {
              buildCommand = if inputs.self.sourceInfo ? rev then ''
                ${old.buildCommand}
                echo ${inputs.self.sourceInfo.rev} > $out/config-commit;
              '' else
                old.buildCommand;
            });
    in {
      homeConfigurations =
        lib.mapAttrs (_: buildHomeManager) (flattenAttrs machines);
      homeModes =
        lib.mapAttrs buildModesForHost { inherit (machines) zeus apollo; };
    });
}
