{ ... }: {
  home.file.".config/jali/config.py".text =
    builtins.readFile ../common/secret/jaliconfig.py;
  home.packages = builtins.attrValues (import ../pkgs).accounting-pkgs;
}
