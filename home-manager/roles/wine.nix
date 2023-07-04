{
  pkgs,
  config,
  ...
}:
let
  wine_dir = "${config.home.homeDirectory}/.volatile/wine_disk";
  wine = pkgs.wineWowPackages.staging;
in
{
  home = {
    sessionVariables = {
      WINEDEBUG = "-all";
      WINEARCH = "win64";
      WINEPREFIX = wine_dir;
      STAGING_SHARED_MEMORY = 1;
      WINEESYNC = 1;
    };
    packages = builtins.attrValues { inherit wine; };
  };
}
