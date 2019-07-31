{ config, pkgs, lib, ... }:
with lib;

let

  me = config.m-0.private.me;

in {
  options = {
    m-0.laptop.enable = mkOption {
      type = types.bool;
      default = false;
    };
  };
  config = mkIf config.m-0.laptop.enable {
    networking = { networkmanager.enable = true; };
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
    nixpkgs.config.allowUnfree = true;

    services = {
      teamviewer.enable = true;
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
        displayManager.auto = {
          enable = true;
          user = "maralorn";
        };
      };
    };
  };
}
