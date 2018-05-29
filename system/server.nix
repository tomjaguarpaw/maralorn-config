{ config, pkgs, ... }:

{
  options = {
    m-0.server.enable = mkOption {
      type = types.bool;
    };
  };

  config = mkIf config.m-0.server.enable {
    nix = {
      gc = {
         automatic = true;
         options = "--delete-older-than 5d";
      };
      optimise.automatic = true;
    };
    system.autoUpgrade.enable = true;
    system.autoUpgrade.dates = "22:00";
  };
}
