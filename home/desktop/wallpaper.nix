{ lib, pkgs, config, ... }:
let
  inherit (import ../../lib) writeHaskellScript;
  randomWallpaper = writeHaskellScript {
    name = "random-wallpaper";
    imports = [ "System.Random" ];
    bins = [ pkgs.coreutils pkgs.sway ];
  } ''
    main = do
       (lines . decodeUtf8 -> files) <- ls "/home/maralorn/.wallpapers" |> captureTrim
       ((files Unsafe.!!) -> file) <- getStdRandom $ randomR (0, length files - 1)
       cp ([i|/home/maralorn/.wallpapers/#{file}|] :: String) "/home/maralorn/volatile/wallpaper.jpg"
       swaymsg "output * bg /home/maralorn/volatile/wallpaper.jpg fill"
  '';
in {

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
