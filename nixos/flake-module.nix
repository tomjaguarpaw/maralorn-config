{
  withSystem,
  lib,
  inputs,
  ...
}: {
  flake = {
    nixosConfigurations = withSystem "x86_64-linux" ({
      system,
      self',
      pkgs,
      ...
    }: let
      machines = builtins.attrNames (builtins.readDir ./machines);
      makeSystem = name:
        pkgs.nixos {
          imports = [
            (import (./. + "/machines/${name}/configuration.nix") inputs)
            inputs.secrets.nixosModules.default
            inputs.self.nixosModules.unstableNFTables
          ];
        };
    in
      lib.genAttrs machines makeSystem);
    nixosModules.unstableNFTables = _: {
      disabledModules = [
        "services/networking/firewall.nix"
        "services/networking/nftables.nix"
        "services/networking/nat.nix"
        "services/networking/redsocks.nix"
        "services/networking/miniupnpd.nix"
        "services/audio/roon-server.nix"
        "services/audio/roon-bridge.nix"
      ];
      imports = let
        networkingModule = name: "${inputs.nixos-unstable}/nixos/modules/services/networking/${name}.nix";
      in [
        # nftables using module not available in 22.11.
        (networkingModule "firewall-iptables")
        (networkingModule "firewall-nftables")
        (networkingModule "firewall")
        (networkingModule "nat-iptables")
        (networkingModule "nat-nftables")
        (networkingModule "nat")
        (networkingModule "nftables")
      ];
    };
  };
}
