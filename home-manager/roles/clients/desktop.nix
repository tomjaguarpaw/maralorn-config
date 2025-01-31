{
  lib,
  config,
  pkgs,
  ...
}:
{
  xdg = {
    configFile."mimeapps.list".force = true;
    enable = true;
    mime.enable = true;
    desktopEntries.neomutt =
      let
        handler = pkgs.writeShellScript "mailto-handler" ''
          exec ${lib.getExe config.programs.neomutt.package} "''${1/mailto:\?/mailto:read@maralorn.de?}"
        '';
      in
      {
        name = "Neomutt";
        genericName = "Mail client";
        exec = "${config.home.sessionVariables.TERMINAL} ${handler} %U";
        categories = [
          "Network"
          "Email"
        ];
        mimeType = [ "x-scheme-handler/mailto" ];
      };
    mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/mailto" = [ "neomutt.desktop" ];
        "application/pdf" = [ "org.gnome.Evince.desktop" ];
        "x-scheme-handler/http" = [ "firefox.desktop" ];
        "x-scheme-handler/https" = [ "firefox.desktop" ];
        "x-scheme-handler/chrome" = [ "firefox.desktop" ];
        "text/html" = [ "firefox.desktop" ];
        "application/x-extension-htm" = [ "firefox.desktop" ];
        "application/x-extension-html" = [ "firefox.desktop" ];
        "application/x-extension-shtml" = [ "firefox.desktop" ];
        "application/xhtml+xml" = [ "firefox.desktop" ];
        "application/x-extension-xhtml" = [ "firefox.desktop" ];
        "application/x-extension-xht" = [ "firefox.desktop" ];
      };
    };
    userDirs = {
      enable = true;
      createDirectories = false;
      desktop = "$HOME";
      download = "$HOME";
      documents = "$HOME/media/documents/aktuell/";
      music = "$HOME/media/audio";
      pictures = "$HOME/media/images";
      videos = "$HOME/media/video";
    };
  };
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
      nautilus

      # media
      deluge
      vlc
      esphome # To flash devices
      esptool # provides esptool.py

      lm_sensors
      brillo
      xwayland
      xdg-utils
      libnotify
      shotcut
      audacity
      wl-clipboard
      chrysalis
      ;
  };

  gtk = {
    enable = true;
    gtk3.bookmarks = [ "ftp://home.local.maralorn.de" ];
  };
}
