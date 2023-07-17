{ pkgs, config, ... }:
let
  modeFile = "${config.home.homeDirectory}/.mode";
  wallPapers = "${config.home.homeDirectory}/media/images/wallpapers";
  randomWallpaper =
    pkgs.writeHaskellScript
      {
        name = "random-wallpaper";
        imports = [ "System.Random" ];
        bins = [
          pkgs.coreutils
          pkgs.glib
        ];
      }
      ''
        main = do
           mode <- cat "${modeFile}" |> captureTrim
           (lines . decodeUtf8 -> files) <- ls ([i|${wallPapers}/#{mode}|] :: String) |> captureTrim
           ((files Unsafe.!!) -> file) <- getStdRandom $ randomR (0, length files - 1)
           (decodeUtf8 -> current) <- gsettings "get" "org.gnome.desktop.background" "picture-uri" |> captureTrim
           let new = [i|file:///${wallPapers}/#{mode}/#{file}|] :: String
           when (new /= current) $ do
             gsettings "set" "org.gnome.desktop.background" "picture-uri-dark" new
             gsettings "set" "org.gnome.desktop.screensaver" "picture-uri" new
      ''
  ;
in
{
  home.packages = [ randomWallpaper ];
  systemd.user = {
    services.random-wallpaper = {
      Unit = {
        Description = "Random Wallpaper";
      };
      Service = {
        ExecStart = "${randomWallpaper}/bin/random-wallpaper";
        Type = "oneshot";
      };
    };
    timers.random-wallpaper = {
      Timer = {
        OnCalendar = "*:00/30:00";
        OnActiveSec = 1;
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
}
