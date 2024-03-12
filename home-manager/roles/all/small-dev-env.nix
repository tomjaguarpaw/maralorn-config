{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      nil # nix language server
      gh
      nix-top
      nixfmt-rfc-style
      ;
  };
}
