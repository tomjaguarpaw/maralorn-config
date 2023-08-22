{ pkgs, ... }:
{
  home = {
    packages = builtins.attrValues {
      inherit (pkgs)
        nil # nix
      ;
      inherit (pkgs)
        lazygit
        gh
        nix-top
        nixfmt
      ;
    };
  };
}
