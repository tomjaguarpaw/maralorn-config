{ pkgs, lib, config, ... }:
with lib;
{

options.m-0.graphical.enable = mkEnableOption "Window Manager";

#imports = if config.m-0.graphical.enable then [ ./i3.nix ] else [];

config = mkIf config.m-0.graphical.enable {
  m-0 = {
    workspaces = [
      "tasks"
      "chat"
      "mail"
      "code"
      "research"
      "work"
      "ccc"
      "orga"
      "leisure"
      "config"
    ];
    terminal = "${pkgs.st}/bin/st";
    colors = {
      "foreground" = "#dddbff";
      "background" = "#05004a";
      "black" = "#030031";
      "brightBlack" = "#55508f";
      "red" = "#e34b4f";
      "brightRed" = "#e34b4f";
      "green" = "#67b779";
      "brightGreen" = "#45b75e";
      "yellow" = "#ff9c00";
      "brightYellow" = "#ff9c00";
      "blue" = "#5c67ff";
      "brightBlue" = "#5c67ff";
      "magenta" = "#cb85ff";
      "brightMagenta" = "#cb85ff";
      "cyan" = "#17d0f4";
      "brightCyan" = "#17d0f4";
      "white" = "#dddbff";
      "brightWhite" = "#ffffff";
    };
  };
  home = {
    packages = with pkgs; [
      tasktree
      st
    ];
    keyboard = {
      layout = "de";
      variant = "neo";
      options = [ "altwin:swap_lalt_lwin" ];
    };
  };
  gtk = {
    enable = true;
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    theme = {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
  };
  xdg.enable = true;
  services = {
    compton = {
      enable = true;
      fade = true;
      fadeDelta = 5;
    };
    gnome-keyring = {
      enable = true;
      components = [
        "secrets"
      ];
    };
    random-background = {
      enable = true;
      imageDirectory = "%h/data/aktuell/media/bilder/wallpaper/";
      interval = "15minutes";
    };
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 900;
      defaultCacheTtlSsh = 900;
    };
    redshift = {
      enable = true;
      temperature.day = 6500;
      latitude = "49.86667";
      longitude = "8.65";
    };
    screen-locker = {
      enable = true;
      lockCmd = "${pkgs.i3lock}/bin/i3lock -n -f -i ~/data/aktuell/media/bilder/lockscreen.png";
    };
  };
  xsession.enable = true;
};

}
