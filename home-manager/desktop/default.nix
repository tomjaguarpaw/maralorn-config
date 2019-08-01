{ pkgs, lib, config, ... }:
let inherit (import ../../common/pkgs.nix) desktop-pkgs;
in {

  imports =
    [ ./i3.nix ./rofi.nix ./ssh-agent.nix ./eventd.nix ./sleep-nag.nix ];
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
    terminal = "${desktop-pkgs.terminal}/bin/terminal";
    colors = {
      "foreground" = "#dddbff";
      "background" = "#000000";
      "black" = "#000000";
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
  xsession.initExtra = "xsetroot -solid black";
  home = {
    packages = builtins.attrValues desktop-pkgs;
    keyboard = {
      layout = "de";
      variant = "neo";
      options = [ "altwin:swap_lalt_lwin" ];
    };
  };
  programs.urxvt = {
    enable = true;
    package = desktop-pkgs.urxvt;
    fonts = [ "6x13" ];
    keybindings = {
      "C-1" = "command:\\033]710;6x13\\007";
      "C-2" = "command:\\033]710;10x20\\007";
      "C-3" = "command:\\033]710;xft:Roboto Mono Nerd Font:size=16\\007";
      "C-4" = "command:\\033]710;xft:Roboto Mono Nerd Font:size=24\\007";
      "C-f" = "matcher:select";
      "C-g" = "matcher:last";
    };
    extraConfig = {
      tintColor = config.m-0.colors.background;
      perl-ext = "default,matcher,clipboard-osc";
      url-launcher = "firefox";
      foreground = config.m-0.colors.foreground;
      background = config.m-0.colors.background;
      color0 = config.m-0.colors.black;
      color1 = config.m-0.colors.red;
      color2 = config.m-0.colors.green;
      color3 = config.m-0.colors.yellow;
      color4 = config.m-0.colors.blue;
      color5 = config.m-0.colors.magenta;
      color6 = config.m-0.colors.cyan;
      color7 = config.m-0.colors.white;
      color8 = config.m-0.colors.brightBlack;
      color9 = config.m-0.colors.brightRed;
      color10 = config.m-0.colors.brightGreen;
      color11 = config.m-0.colors.brightYellow;
      color12 = config.m-0.colors.brightBlue;
      color13 = config.m-0.colors.brightMagenta;
      color14 = config.m-0.colors.brightCyan;
      color15 = config.m-0.colors.brightWhite;
    };
    scroll = {
      bar.enable = false;
      lines = 0;
    };
  };
  gtk = {
    enable = true;
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    theme = {
      name = "Arc";
      package = pkgs.arc-theme;
    };
  };
  services = {
    nextcloud-client.enable = true;
    redshift = {
      enable = true;
      temperature.day = 6500;
      latitude = "49.86667";
      longitude = "8.65";
    };
    screen-locker = {
      enable = true;
      lockCmd =
        "${pkgs.i3lock}/bin/i3lock -n -f -i ~/data/aktuell/media/bilder/lockscreen.png";
    };
  };
  xsession.enable = true;
}
