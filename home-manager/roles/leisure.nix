{ pkgs, lib, config, ... }:
let
  serien = pkgs.writeShellScript "serien.rss" ''
    cat <<EOF
    <?xml version="1.0"?>
    <rss version="2.0">
       <channel>
          <title>Neue Serien Folgen</title>
          <link>https://www.pogdesign.co.uk/cat</link>
          <description>Neue Serien Folgen via TV Calendar</description>
    EOF
    khal list now 14d -a Serien -df "" -f '<item><title>{title}</title><description>{description}</description><guid>{uid}</guid><pubDate>{start-date}</pubDate></item>'
    cat <<EOF
       </channel>
    </rss>
    EOF
  '';
  watchfeedsConfig = ''
    datetime-format "%Y-%m-%d"
    cleanup-on-quit no
  '';
  watchfeeds = [
    # Haskell
    "https://www.youtube.com/feeds/videos.xml?channel_id=UCI1Z201n-8OelkSg0DVOsng" # Tweag
    "https://www.youtube.com/feeds/videos.xml?channel_id=UCxVE_2I_fsOe3Mgn_QlXqbQ" # Nomeata

    "http://www.zdf.de/rss/podcast/video/zdf/comedy/die-anstalt"

    "https://www.zdf.de/rss/zdf/show/mai-think-x-die-show"
    "https://www.youtube.com/feeds/videos.xml?channel_id=UCyHDQ5C6z1NDmJ4g6SerW8g" # Mailab

    "https://www.youtube.com/feeds/videos.xml?channel_id=UC2C_jShtL725hvbm1arSV9w" # GCP Grey

    "https://www.youtube.com/feeds/videos.xml?channel_id=UCpXBGqwsBkpvcYjsJBQ7LEQ" # Critical Role
    "exec:${serien}"
  ];
  newsConfig = ''
    ${watchfeedsConfig}
    urls-source "ocnews"
    ocnews-url "https://cloud.maralorn.de"
    ocnews-login "maralorn"
    ocnews-password "${(pkgs.privateValue { adminpass =""; } "nextcloud-admin").adminpass}"
  '';
in
{
  xdg.configFile."newsboat/urls".text = lib.concatStringsSep "\n" watchfeeds;
  home = {
    packages = builtins.attrValues {
      news = pkgs.writeShellScriptBin "news" "${pkgs.newsboat}/bin/newsboat -r -C ${builtins.toFile "news-config" newsConfig}";
      watchfeeds = pkgs.writeShellScriptBin "watchfeeds" "${pkgs.newsboat}/bin/newsboat -r -C ${builtins.toFile "watchfeeds-config" watchfeedsConfig}";
    };
  };
}
