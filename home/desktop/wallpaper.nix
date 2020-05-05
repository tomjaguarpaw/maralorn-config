{ lib, pkgs, config, ... }:
let
  inherit (import ../../lib) writeHaskellScript;
  randomWallpaper = writeHaskellScript {
    name = "random-wallpaper";
    imports = [ "System.Random" ];
    bins = [ pkgs.coreutils pkgs.sway ];
  } ''
    main = do
       mode <- cat "/home/maralorn/tmp/mode" |> captureTrim
       (lines . decodeUtf8 -> files) <- ls ([i|/home/maralorn/.wallpapers/#{mode}|] :: String) |> captureTrim
       ((files Unsafe.!!) -> file) <- getStdRandom $ randomR (0, length files - 1)
       cp ([i|/home/maralorn/.wallpapers/#{mode}/#{file}|] :: String) "/home/maralorn/volatile/wallpaper.jpg"
       swaymsg "output * bg /home/maralorn/volatile/wallpaper.jpg fill"
  '';
in {
  home.packages = [ randomWallpaper ];
  systemd.user = {
    services.random-wallpaper = {
      Unit = { Description = "Random Wallpaper"; };
      Service = {
        ExecStart = "${randomWallpaper}/bin/random-wallpaper";
        Type = "oneshot";
      };
    };
    timers.random-wallpaper = {
      Timer = { OnCalendar = "*:00/5:00"; };
      Install = { WantedBy = [ "timers.target" ]; };
    };
  };

}
