{ lib, pkgs, config, ... }:
with lib; {

  options.m-0.accounting.enable = mkEnableOption "Accounting";
  options.m-0.accounting.config = mkOption { type = types.str; };

  config = mkIf config.m-0.accounting.enable {
    home.file.".config/jali/config.py".text = config.m-0.accounting.config;
    home.packages = builtins.attrValues (import ../../../pkgs).accounting-pkgs;
  };

}
