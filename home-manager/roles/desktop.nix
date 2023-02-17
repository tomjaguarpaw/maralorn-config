{pkgs, ...}: let
  inherit (import ../../lib) colors;
in {
  imports = [./mpv];
  m-0.colors = colors;
  home = {
    packages = builtins.attrValues {
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
      inherit
        (pkgs)
        # web
        
        chromium
        mumble
        upower
        speedtest-cli
        acpi
        # tools & office
        
        feh
        gimp
        imagemagick
        libreoffice-fresh
        xournal
        musescore
        handbrake
        evince
        abcde
        beets
        zbar
        # media
        
        ncpamixer
        pavucontrol
        playerctl
        deluge
        gmpc
        vlc
        yt-dlp
        spotdl
        ffmpeg
        syncplay
        esphome
        # To flash devices
        
        esptool
        # provides esptool.py
        
        lm_sensors
        xwayland
        xdg_utils
        libnotify
        kassandra-standalone
        shotcut
        audacity
        paprefs
        wl-clipboard
        dconf2nix
        chrysalis
        ;
      inherit
        (pkgs.gnome)
        dconf-editor
        gnome-tweaks
        adwaita-icon-theme
        gnome-session
        ;
    };

    file.".zprofile".text = ". $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh";
  };
  gtk = {
    enable = true;
    iconTheme = {
      name = "Tela-circle";
      package = pkgs.tela-circle-icon-theme;
    };
    cursorTheme = {
      name = "Catppuccin-Mocha-Blue-Cursors";
      package = pkgs.catppuccin-cursors.mochaBlue;
    };
    theme = {
      name = "Catppuccin-Mocha-Compact-Blue-Dark";
      package = pkgs.unstable.catppuccin-gtk.override {
        variant = "mocha";
        size = "compact";
        tweaks = ["rimless"];
      };
    };
    gtk3.bookmarks = [
      "ftp://fluffy.lo.m-0.eu"
    ];
  };
}
