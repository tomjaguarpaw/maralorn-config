{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs) qjackctl supercollider-with-sc3-plugins pulsar;
  };
}
