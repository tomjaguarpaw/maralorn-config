{ lib, pkgs, config, ... }:
let
  inherit (import ../../lib) writeHaskellScript;
  randomWallpaper = writeHaskellScript {
    name = "random-wallpaper";
    imports = [ "System.Random" ];
    bins = [ pkgs.coreutils pkgs.sway ];
  } ''
    main = forever $ do
       files <- fmap (lines . decodeUtf8) $ ls "/home/maralorn/.wallpapers" |> captureTrim
       file <- fmap (files Unsafe.!!) $ getStdRandom $ randomR (0, length files - 1)
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
      Timer = { OnCalendar = "*:*:0/5"; };
      Install = { WantedBy = [ "timers.target" ]; };
    };
  };

}
