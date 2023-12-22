{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      pamixer
      ncpamixer
      pavucontrol
      pulseaudio
      playerctl
      yt-dlp
      spotdl
      ffmpeg
      paprefs
      ;
  };
}
