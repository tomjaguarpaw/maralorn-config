{ pkgs, ... }:
{
  home.packages = builtins.attrValues { inherit (pkgs) gamemode mangohud; };
}
