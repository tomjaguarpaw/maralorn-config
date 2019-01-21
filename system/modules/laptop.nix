{ config, pkgs, lib, ... }:
with lib;

let

  me = config.m-0.private.me;

in
{
  options = {
    m-0.laptop.enable = mkOption {
      type = types.bool;
      default = false;
    };
  };
  config = mkIf config.m-0.laptop.enable {
    networking = {
      networkmanager.enable = true;
    };
    i18n.consoleKeyMap = "neo";

    sound.enable = true;
    hardware.opengl = {
      driSupport = true;
      driSupport32Bit = true;
    };
    hardware.pulseaudio = {
      enable = true;
      tcp = {
        enable = true;
        anonymousClients.allowedIpRanges = [ "127.0.0.1" "::1" ];
      };
    };

    services = {
      unbound = {
        enable = true;
        extraConfig = ''
          server:
            domain-insecure: dn42.

          forward-zone:
            name: "dn42."
            forward-addr: 172.23.0.53
        '';
      };
      mpd = {
          enable = true;
          user = me.user;
          group = "users";
          network.listenAddress = "::1";
          musicDirectory = "/home/${me.user}/data/aktuell/media/musik";
          extraConfig = ''
          audio_output {
                type "pulse"
                name "Pulseaudio"
                server "localhost"
          }
          '';
      };
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
