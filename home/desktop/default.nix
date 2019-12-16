{ pkgs, lib, config, ... }:
let
  inherit (import ../../pkgs) desktop-pkgs;
  inherit (import ../../lib) colors;
in {
  imports =
    [ ./sway.nix ./wallpaper.nix ./rofi.nix ./ssh-agent.nix ./sleep-nag.nix ];
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
    colors = colors;
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
      musicDirectory = "${config.home.homeDirectory}/media/audio";
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
