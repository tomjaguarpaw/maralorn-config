{ pkgs, lib, config, ... }:
{
  home = {
    file = {
      "newsboat-config" = {
        target = ".newsboat/config";
        text = "datetime-format \"%Y-%m-%d\"";
      };
      "newsboat-urls" = {
        target = ".newsboat/urls";
        text = lib.concatStringsSep "\n" [
          # Haskell
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCI1Z201n-8OelkSg0DVOsng" # Tweag
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCxVE_2I_fsOe3Mgn_QlXqbQ" # Nomeata

          "http://www.zdf.de/rss/podcast/video/zdf/comedy/die-anstalt"

          "https://www.zdf.de/rss/zdf/show/mai-think-x-die-show"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCyHDQ5C6z1NDmJ4g6SerW8g" # Mailab

          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2C_jShtL725hvbm1arSV9w" # GCP Grey
        ];
      };
    };

    packages = builtins.attrValues rec {
      zoom = pkgs.zoom-us.overrideAttrs (old: {
        postFixup = old.postFixup + ''
          wrapProgram $out/bin/zoom-us --unset XDG_SESSION_TYPE
          wrapProgram $out/bin/zoom --unset XDG_SESSION_TYPE
        '';
      });

      inherit (pkgs.gnome) nautilus;
      inherit (pkgs.xorg) xbacklight;
      inherit (pkgs)
        # web
        chromium

        skypeforlinux google-chrome

        mumble upower speedtest-cli acpi

        anki

        # tools & office
        feh gimp imagemagick libreoffice-fresh xournal musescore handbrake evince
        abcde beets zbar

        # media
        ncpamixer pavucontrol deluge gmpc vlc mpv youtubeDL syncplay

        newsboat;
    };
  };
}
