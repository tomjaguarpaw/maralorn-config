{ pkgs, config, lib, ... }:
with lib; {

  imports = [ ./admin.nix ./boot-key.nix ];

  # So that boot does not fill up with old kernels
  boot.loader.grub.configurationLimit = 5;

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  security.sudo.extraConfig =
    "\n    Defaults timestamp_type=global, timestamp_timeout=15\n  ";

  services = { sshd.enable = true; };

  nix.nixPath = [ "nixos-config=/etc/nixos/configuration.nix" ];

  environment = {
    # Put these into an extra file so the essential packages can also be included on non selfadminstrated systems from home-manager
    systemPackages = builtins.attrValues ({
      inherit (import ../../lib/update-system.nix
      config.system.build.nixos-rebuild)
        update-system system-maintenance;
    } // (import ../../pkgs).system-pkgs);
    sessionVariables = { TERMINFO = "/run/current-system/sw/share/terminfo"; };
  };

  programs = {
    mtr.enable = true;
    zsh = {
      enable = true;
      autosuggestions.enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
    };
  };

}
