{config, lib, ...}:
{
  systemd.user.services.waybar = {
    Unit.Description = "Waybar";
    Service = {
      ExecStart = lib.getExe config.programs.waybar.package;
      Restart = "always";
      RestartSec = "10s";
    };
    Install.WantedBy = ["graphical-session.target"];
  };
  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        position = "bottom";
        layer = "bottom";
        modules-left = ["tray"];
        exclusive = false;
        margin = "5";
      };
    };
    style = ''
      window#waybar {
        background: rgba(0,0,0,0);
      }
    '';
  };
}
