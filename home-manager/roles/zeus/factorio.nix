{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    factorio = pkgs.factorio-space-age.override {
      username = "maralorn";
      token = pkgs.privateValue "" "factorio";
    };
  };
}
