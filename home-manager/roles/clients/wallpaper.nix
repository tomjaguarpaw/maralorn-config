{ pkgs, ... }:

{
  xdg.configFile.wallpaper.source = pkgs.fetchurl {
    url = "https://images-assets.nasa.gov/image/PIA12832/PIA12832~orig.jpg";
    hash = "sha256-3yhQlzspkpJL5aoeC7kuiJ5I5SJqv2eixwxI8PQMeVU=";
  };
}
