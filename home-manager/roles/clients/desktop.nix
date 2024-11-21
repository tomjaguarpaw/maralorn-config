{ pkgs, ... }:
{
  # This fixes border drawing but makes neo wonky. sessionVariables.NIXOS_OZONE_WL = "1";
  home.packages = builtins.attrValues {
    zoom = pkgs.zoom-us.overrideAttrs (old: {
      postFixup =
        old.postFixup
        + ''
          wrapProgram $out/bin/zoom-us --unset XDG_SESSION_TYPE
          wrapProgram $out/bin/zoom --unset XDG_SESSION_TYPE
        '';
    });
    mic-check = pkgs.writeShellScriptBin "mic-check" ''
      echo "Activating loopback!"
      ${pkgs.pulseaudio}/bin/pactl load-module module-loopback
      echo "Can your hear yourself? Fix audio setup! Then press enter …"
      read
      echo "Deactivating loopback!"
      ${pkgs.pulseaudio}/bin/pactl unload-module module-loopback
      echo "Continuing …"
    '';

    inherit (pkgs.gnome) nautilus;
    inherit (pkgs.xorg) xbacklight;
    inherit (pkgs)
      # web
      chromium
      mumble
      upower
      speedtest-cli
      acpi

      # tools & office
      klog-time-tracker
      feh
      gimp
      imagemagick
      libreoffice-fresh
      xournal
      musescore
      handbrake
      evince
      abcde
      zbar
      graphia
      pdfpc

      # media
      deluge
      vlc
      esphome # To flash devices
      esptool # provides esptool.py

      lm_sensors
      xwayland
      xdg-utils
      libnotify
      shotcut
      audacity
      wl-clipboard
      dconf2nix
      chrysalis
      ;
    inherit (pkgs.gnome) dconf-editor;
  };

  gtk = {
    enable = true;
    gtk3.bookmarks = [ "ftp://home.local.maralorn.de" ];
  };
}
