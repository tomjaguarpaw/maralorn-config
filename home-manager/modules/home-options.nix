{ lib, config, pkgs, ... }:

with lib;
{
  options = {
    common = {
      terminal = mkOption {
        default = "urxvt";
        type = types.str;
      };
      colors = mkOption {
        default = {};
        type = types.attrs;
      };
      workspaces = mkOption {
        default = [ "configure some workspaces" ];
        type = types.listOf types.str;
      };
    };
  };
}
