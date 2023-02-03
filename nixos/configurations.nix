{
  withSystem,
  lib,
  inputs,
  ...
}: {
  flake = withSystem "x86_64-linux" ({
    inputs',
    system,
    ...
  }: let
    networkingModule = name: "${inputs.nixos-unstable}/nixos/modules/services/networking/${name}.nix";
    modules = [
      # nftables using module not available in 22.11.
      (networkingModule "firewall-iptables")
      (networkingModule "firewall-nftables")
      (networkingModule "firewall")
      (networkingModule "nat-iptables")
      (networkingModule "nat-nftables")
      (networkingModule "nat")
      (networkingModule "nftables")
      (_: {
        disabledModules = [
          "services/networking/firewall.nix"
          "services/networking/nftables.nix"
          "services/networking/nat.nix"
          "services/networking/redsocks.nix"
          "services/networking/miniupnpd.nix"
          "services/audio/roon-server.nix"
          "services/audio/roon-bridge.nix"
        ];
      })
    ];
    makeSystem = name:
      inputs'.nixos-stable.legacyPackages.nixos {
        imports =
          [
            (import (./. + "/machines/${name}/configuration.nix") inputs)
            inputs.secrets.nixosModules.secrets
            inputs.self.nixosModules.insertOverlays
          ]
          ++ modules;
      };
  in {
    nixosModules.insertOverlays = _: {
      _module.args = {
        flake-inputs = inputs // {inherit modules;};
        flake-inputs' = inputs';
      };
      nixpkgs.overlays =
        [
          (_: _:
            {
              unstable = inputs'.nixos-unstable.legacyPackages;
              unfree = import inputs.nixos-stable {
                inherit system;
                config = {
                  allowUnfree = true;
                  android_sdk.accept_license = true;
                };
              };
              unstableUnfree = import inputs.nixos-unstable {
                config.allowUnfree = true;
                inherit system;
              };
            }
            // inputs.secrets.private)
          inputs.self.overlays.addMyHaskellPackages
        ]
        ++ import ../overlays {inherit lib;};
    };
    nixosConfigurations = lib.genAttrs ["zeus" "apollo" "hera" "fluffy"] makeSystem;
  });
}
