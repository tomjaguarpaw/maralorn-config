{pkgs, ...}:
{
  home.packages = builtins.attrValues {
    factorio = pkgs.factorio.override {
      username = "maralorn";
      token = pkgs.privateValue "" "factorio";
    };
  };
}
