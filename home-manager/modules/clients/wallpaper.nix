{
  pkgs,
  lib,
  config,
  ...
}:
let
  inherit (config.home) homeDirectory;
  modeFile = "${homeDirectory}/.mode";
  wallPapers = "${homeDirectory}/media/images/wallpapers";
  hyprpaperConfig = "${homeDirectory}/.config/hypr/hyprpaper.conf";
  fallback = pkgs.fetchurl {
    url = "https://wallpapers.com/images/hd/pink-space-nebula-shp7fs58j0or29qo.jpg";
    hash = "sha256-gahXC/kx8SL6XaZBli1tNdIFJn8M3SES3IWkPUp1O8k=";
  };
  wallpaper-daemon =
    pkgs.writeHaskellScript
      {
        name = "wallpaper-daemon";
        imports = [
          "System.Random"
          "Control.Exception (handle)"
        ];
        bins = [
          pkgs.coreutils
          pkgs.hyprpaper
        ];
      }
      ''
        main = do
           file <- handle (\(_ :: SomeException) -> pure "${fallback}") do
             mode <- cat "${modeFile}" |> captureTrim
             (lines . decodeUtf8 -> files) <- ls ([i|${wallPapers}/#{mode}|] :: String) |> captureTrim
             ((files Unsafe.!!) -> file) <- getStdRandom $ randomR (0, length files - 1)
             pure ([i|${wallPapers}/#{mode}/#{file}|] :: Text)
           writeFile "${hyprpaperConfig}" [i|preload = #{file}\nwallpaper = ,#{file}|];
           hyprpaper
      '';
in
{
  systemd.user.services.wallpaper = {
    Unit.Description = "Wallpaper Daemon";
    Service.ExecStart = lib.getExe wallpaper-daemon;
  };
}
