{ pkgs, config, lib, ... }:
with lib;
{

  imports = [ ./admin.nix ];

  # So that boot does not fill up with old kernels
  boot.loader.grub.configurationLimit = 5;

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  security.sudo.extraConfig = "
    Defaults timestamp_type=global, timestamp_timeout=15
  ";

  services = {
    sshd.enable = true;
  };

  nix.nixPath = [ "nixos-config=/etc/nixos/configuration.nix" ];

  environment = {
    # Put these into an extra file so the essential packages can also be included on non selfadminstrated systems from home-manager
    systemPackages = let essentials = import ../common/essentials.nix;
  in essentials.core ++ essentials.extra ++ (builtins.attrValues {
        inherit (import ./test-lib.nix) test-system-config test-home-config test-and-bump-config;
        inherit (import ../common/lib.nix) home-manager;
        inherit (import ./update-lib.nix config.system.build.nixos-rebuild) update-system system-maintenance;
      });
    sessionVariables = {
      TERMINFO = "/run/current-system/sw/share/terminfo";
    };
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
