{ pkgs, lib, ... }:
let
  tasktree = with pkgs; callPackage ../../packages/tasktree {};
in {
  imports = [
    ./i3.nix
    ../../modules/home-options.nix
  ];
  common = {
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
    terminal = "xterm";
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
        "ssh"
      ];
    };
    random-background = {
      enable = true;
      imageDirectory = "%h/data/aktuell/media/bilder/wallpaper/";
      interval = "15minutes";
    };
    gpg-agent = {
      enable = true;
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
}
