{ pkgs, ... }: {
  xdg.configFile."jali/config.py".source = pkgs.privateFile "jaliconfig.py";
  home.packages = builtins.attrValues pkgs.accounting-pkgs;
}
