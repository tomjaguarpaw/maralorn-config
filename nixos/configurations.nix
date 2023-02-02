flake-inputs: let
  inherit (flake-inputs.nixos-stable) lib;
  networkingModule = name: "${flake-inputs.nixos-unstable}/nixos/modules/services/networking/${name}.nix";
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
    lib.nixosSystem {
      modules =
        [
          (import (./. + "/machines/${name}/configuration.nix") flake-inputs)
          flake-inputs.secrets.nixosModules.secrets
          (_: {config._module.args.flake-inputs = flake-inputs // {inherit modules;};})
        ]
        ++ modules;
    };
in
  lib.genAttrs ["zeus" "apollo" "hera" "fluffy"] makeSystem
