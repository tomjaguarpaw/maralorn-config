{ pkgs, lib, ... }:
{
  home.packages = [
    pkgs.randomWallpaper
    pkgs.hyprpaper
  ];

  xdg.configFile."hypr/hyprpaper.conf".text = "\n     preload = /home/maralorn/media/images/wallpapers/code/raf,750x1000,075,t,101010 01c5ca27c6.u3.jpg\n     wallpaper = ,/home/maralorn/media/images/wallpapers/code/raf,750x1000,075,t,101010 01c5ca27c6.u3.jpg\n  ";

  systemd.user = {
    services.random-wallpaper = {
      Unit = {
        Description = "Random Wallpaper";
      };
      Service = {
        ExecStart = lib.getExe pkgs.randomWallpaper;
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
