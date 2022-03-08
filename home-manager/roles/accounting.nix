{pkgs, ...}: {
  home.packages = builtins.attrValues pkgs.accounting-pkgs;
}
