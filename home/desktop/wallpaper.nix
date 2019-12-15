{ lib, pkgs, config, ... }:
let
  inherit (import ../../lib) writeHaskellScript;
  randomWallpaper = writeHaskellScript {
    name = "random-wallpaper";
    imports = [ "System.Random" "Control.Concurrent" ];
    bins = [ pkgs.coreutils pkgs.sway ];
  } ''
    main = forever $ do
       files <- fmap (lines . decodeUtf8) $ ls "/home/maralorn/.wallpapers" |> captureTrim
       file <- fmap (files Prelude.!!) $ getStdRandom $ randomR (0, length files - 1)
       cp ([i|/home/maralorn/.wallpapers/#{file}|] :: String) "/home/maralorn/volatile/wallpaper.jpg"
       swaymsg "output * bg /home/maralorn/volatile/wallpaper.jpg fill"
       threadDelay 300000000
  '';
in {

  systemd.user = {
    services.random-wallpaper = {
      Unit = { Description = "Random Wallpaper"; };
      Service = {
        ExecStart = "${randomWallpaper}/bin/random-wallpaper";
        Restart = "always";
        RestartSec = "10";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };

}
