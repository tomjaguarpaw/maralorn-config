{pkgs, ...}:
{
  home = {
    # This fixes border drawing but makes neo wonky. sessionVariables.NIXOS_OZONE_WL = "1";
    packages = builtins.attrValues {
      zoom = pkgs.zoom-us.overrideAttrs (
        old: {
          postFixup =
            old.postFixup
            + ''
              wrapProgram $out/bin/zoom-us --unset XDG_SESSION_TYPE
              wrapProgram $out/bin/zoom --unset XDG_SESSION_TYPE
            '';
        }
      );
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

        # media
        deluge
        vlc
        syncplay
        esphome # To flash devices
        esptool # provides esptool.py

        lm_sensors
        xwayland
        xdg_utils
        libnotify
        shotcut
        audacity
        wl-clipboard
        dconf2nix
        chrysalis
        ;
      inherit (pkgs.gnome) dconf-editor;
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
      package = pkgs.catppuccin-gtk.override {
        variant = "mocha";
        size = "compact";
        tweaks = ["rimless"];
      };
    };
    gtk3.bookmarks = ["ftp://athene.lo.m-0.eu"];
  };
}
