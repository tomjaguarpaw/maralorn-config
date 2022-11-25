{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [./mpv];
  home.packages = builtins.attrValues rec {
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
      youtube-dl
      spotdl
      ffmpeg
      syncplay
      ;
  };
}
