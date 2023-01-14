{
  pkgs,
  config,
  ...
}: let
  inherit (import ../../../nix/sources.nix) nixos-unstable;
  networkingModule = name: "${nixos-unstable}/nixos/modules/services/networking/${name}.nix";
in {
  # nftables using module not available in 22.11.
  disabledModules = [
    "services/networking/firewall.nix"
    "services/networking/nftables.nix"
    "services/networking/nat.nix"
    "services/networking/redsocks.nix"
    "services/networking/miniupnpd.nix"
    "services/monitoring/prometheus/exporters.nix"
    "services/audio/roon-server.nix"
    "services/audio/roon-bridge.nix"
  ];

  imports = [
    (networkingModule "firewall-iptables")
    (networkingModule "firewall-nftables")
    (networkingModule "firewall")
    (networkingModule "nat-iptables")
    (networkingModule "nat-nftables")
    (networkingModule "nat")
    (networkingModule "nftables")
  ];

  networking = {
    firewall = {
      enable = true; # It’s the default, but better make sure.
    };
    nftables.enable = true; # Uses firewall variables since 23.05
  };

  boot = {
    plymouth.enable = true;
    loader = {
      timeout = 0;
      grub = {
        backgroundColor = "#000000";
        # So that boot does not fill up with old kernels
        configurationLimit = 5;
      };
    };
    kernel.sysctl."fs.inotify.max_user_watches" = 204800;
  };

  security.sudo.extraConfig = "\n    Defaults timestamp_type=global, timestamp_timeout=15\n  ";

  services.sshd.enable = true;

  nix = {
    nixPath = ["nixos-config=/etc/nixos/configuration.nix"];
    gc = {
      automatic = false;
      options = "-d";
    };
  };

  environment = {
    # Put these into an extra file so the essential packages can also be included on non selfadminstrated systems from home-manager
    systemPackages = builtins.attrValues {
      inherit
        (import ../../../lib/update-system.nix {
          inherit pkgs;
          inherit (config.system.build) nixos-rebuild;
        })
        update-system
        ;
    };
  };

  programs = {
    mtr.enable = true;
  };
}
