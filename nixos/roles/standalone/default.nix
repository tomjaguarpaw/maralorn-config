{ pkgs, config, ... }: {

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
  };

  security.sudo.extraConfig =
    "\n    Defaults timestamp_type=global, timestamp_timeout=15\n  ";

  services.sshd.enable = true;

  nix = {
    nixPath = [ "nixos-config=/etc/nixos/configuration.nix" ];
    gc = {
      automatic = false;
      options = "-d";
    };
  };

  environment = {
    # Put these into an extra file so the essential packages can also be included on non selfadminstrated systems from home-manager
    systemPackages = builtins.attrValues ({
      inherit (import ../../../lib/update-system.nix {
        inherit pkgs;
        nixos-rebuild = config.system.build.nixos-rebuild;
      })
        update-system;
    } // pkgs.system-pkgs);
  };

  programs = {
    mtr.enable = true;
  };

}
