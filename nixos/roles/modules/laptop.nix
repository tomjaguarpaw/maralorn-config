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
    console.keyMap = "neo";

    sound.enable = true;
    hardware.opengl = {
      enable = true;
      driSupport32Bit = true; # for gw2
    };
    hardware.pulseaudio = {
      enable = true;
      support32Bit = true;
      tcp = {
        enable = true;
        anonymousClients.allowedIpRanges = [ "127.0.0.1" "::1" ];
      };
    };
    programs.dconf.enable = true;

    virtualisation.docker.enable = true;
    services = {
      upower.enable = true;
      printing = {
        enable = true;
        drivers = [ pkgs.gutenprint pkgs.hplip ];
      };
      udev.extraRules = ''
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
      '';
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
    };
  };
}
