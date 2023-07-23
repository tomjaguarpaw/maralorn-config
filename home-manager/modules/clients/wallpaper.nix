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
          pkgs.hyprland
        ];
      }
      ''
        main = do
           mode <- cat "${modeFile}" |> captureTrim
           (lines . decodeUtf8 -> files) <- ls ([i|${wallPapers}/#{mode}|] :: String) |> captureTrim
           ((files Unsafe.!!) -> file) <- getStdRandom $ randomR (0, length files - 1)
           let new = [i|${wallPapers}/#{mode}/#{file}|] :: String
           hyprctl "hyprpaper" "preload" new
           hyprctl "hyprpaper" "wallpaper" ([i|DP-2,#{new}|] :: String)
      '';
in
{
  home.packages = [
    randomWallpaper
    pkgs.hyprpaper
  ];

  xdg.configFile."hypr/hyprpaper.conf".text = "\n     preload = /home/maralorn/media/images/wallpapers/code/raf,750x1000,075,t,101010 01c5ca27c6.u3.jpg\n     wallpaper = ,/home/maralorn/media/images/wallpapers/code/raf,750x1000,075,t,101010 01c5ca27c6.u3.jpg\n  ";

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
