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
  wallpaper-daemon =
    pkgs.writeHaskellScript
      {
        name = "wallpaper-daemon";
        imports = [ "System.Random" ];
        bins = [
          pkgs.coreutils
          pkgs.hyprpaper
        ];
      }
      ''
        main = do
           mode <- cat "${modeFile}" |> captureTrim
           (lines . decodeUtf8 -> files) <- ls ([i|${wallPapers}/#{mode}|] :: String) |> captureTrim
           ((files Unsafe.!!) -> file) <- getStdRandom $ randomR (0, length files - 1)
           let new = [i|${wallPapers}/#{mode}/#{file}|] :: Text
           writeFile "${hyprpaperConfig}" [i|preload = #{new}\nwallpaper = ,#{new}|];
           hyprpaper
      '';
in
{
  systemd.user.services.wallpaper = {
    Unit = {
      Description = "Wallpaper Daemon";
    };
    Service = {
      ExecStart = lib.getExe wallpaper-daemon;
    };
  };
}
