{ pkgs, ... }: {
  home.file.".config/jali/config.py".source = ../common/secret/jaliconfig.py;
  home.packages = builtins.attrValues pkgs.accounting-pkgs;
}
