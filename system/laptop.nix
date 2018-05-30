{ config, pkgs, lib, ... }:
with lib;
{
  options = {
    m-0.laptop.enable = mkOption {
      type = types.bool;
    };
  };
  config = mkIf config.m-0.laptop.enable {
    networking = {
      networkmanager.enable = true;
    };
    i18n.consoleKeyMap = "neo";
    hardware.pulseaudio.enable = true;
    services = {
      xserver = {
        enable = true;
        layout = "de";
        xkbVariant = "neo";
        libinput.enable = true;
        desktopManager.gnome3.enable = true;
        displayManager.auto = {
          enable = true;
          user = "maralorn";
        };
        config = ''
          Section "InputClass"
          Identifier "Enable libinput for TrackPoint"
          MatchIsPointer "on"
          Driver "libinput"
          EndSection
        '';
      };
    };
  };
}
