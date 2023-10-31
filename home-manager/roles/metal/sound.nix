{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs)
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
