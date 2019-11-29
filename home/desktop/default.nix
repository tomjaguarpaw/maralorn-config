{ pkgs, lib, config, ... }:
let inherit (import ../../pkgs) desktop-pkgs;
in {
  imports = [
    ./sway.nix
    ./wallpaper.nix
    ./rofi.nix
    ./ssh-agent.nix
    ./eventd.nix
    ./sleep-nag.nix
  ];
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
  home = { packages = builtins.attrValues desktop-pkgs; };
  programs.browserpass.enable = true;
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
    mpd = {
      enable = true;
      network.listenAddress = "::1";
      musicDirectory = "${config.home.homeDirectory}/data/aktuell/media/musik";
      extraConfig = ''
        audio_output {
              type "pulse"
              name "Pulseaudio"
              server "localhost"
        }
      '';
    };
    mpdris2.enable = true;
  };
  systemd.user.services.mpdris2 = {
    Unit.Requires = [ "dbus.service" ];
    Install.WantedBy = [ "default.target" ];
  };
}
