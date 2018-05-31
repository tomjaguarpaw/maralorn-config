{ lib, pkgs, config, ...}:
with lib;
{

options.m-0.accounting.enable = mkEnableOption "Accounting";

config = mkIf config.m-0.accounting.enable {
  home.packages = with pkgs; [
    hledger
    haskellPackages.hledger-ui
    ledger
    jali
  ];
};

}
