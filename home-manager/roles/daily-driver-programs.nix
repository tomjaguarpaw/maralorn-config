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
      deluge
      gmpc
      vlc
      youtubeDL
      spotdl
      id3v2
      ffmpeg
      syncplay
      ;
  };
}
