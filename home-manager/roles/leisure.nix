{
  pkgs,
  lib,
  config,
  ...
}: let
  serien = pkgs.writeShellScript "serien.rss" ''
    cat <<EOF
    <?xml version="1.0"?>
    <rss version="2.0">
       <channel>
          <title>Neue Serien Folgen</title>
          <link>https://www.pogdesign.co.uk/cat</link>
          <description>Neue Serien Folgen via TV Calendar</description>
    EOF
    ${pkgs.khal}/bin/khal list now 14d -a Serien -df "" -f '<item><title>{title}</title><description>{description}</description><guid>{uid}</guid><pubDate>{start-date}</pubDate></item>'
    cat <<EOF
       </channel>
    </rss>
    EOF
  '';
  commands = builtins.mapAttrs (name: {
    config ? "",
    feeds ? [],
  }: let
    configFile = builtins.toFile "${name}-config" ''
      show-read-feeds no
      show-read-articles no
      datetime-format "%Y-%m-%d"
      ${config}
    '';
    urlFile = pkgs.writeText "${name}-urls" (lib.concatStringsSep "\n" feeds);
  in
    pkgs.writeShellScriptBin name "${pkgs.newsboat}/bin/newsboat -r -C ${configFile} -u ${urlFile} -c ~/.local/share/newsboat/${name}-cache.db \"$@\"") {
    watchfeeds = {
      config = ''browser "mpv %u"'';
      feeds = [
        # Haskell
        "https://www.youtube.com/feeds/videos.xml?channel_id=UCI1Z201n-8OelkSg0DVOsng" # Tweag
        "https://www.youtube.com/feeds/videos.xml?channel_id=UCxVE_2I_fsOe3Mgn_QlXqbQ" # Nomeata

        "http://www.zdf.de/rss/podcast/video/zdf/comedy/die-anstalt"

        "https://www.zdf.de/rss/zdf/show/mai-think-x-die-show"
        "https://www.youtube.com/feeds/videos.xml?channel_id=UCyHDQ5C6z1NDmJ4g6SerW8g" # Mailab

        "https://www.youtube.com/feeds/videos.xml?channel_id=UC3vIimi9q4AT8EgxYp_dWIw" # NixOS

        "https://www.youtube.com/feeds/videos.xml?channel_id=UC2C_jShtL725hvbm1arSV9w" # GCP Grey

        "https://www.youtube.com/feeds/videos.xml?channel_id=UCpXBGqwsBkpvcYjsJBQ7LEQ" # Critical Role

        "https://www.youtube.com/feeds/videos.xml?channel_id=UC2PA-AKmVpU6NKCGtZq_rKQ" # Philosophy Tube

        "https://www.youtube.com/feeds/videos.xml?channel_id=UCwRH985XgMYXQ6NxXDo8npw" # Kurzgesagt

        "exec:${serien}"
      ];
    };
    news = {
      config = ''
        urls-source "ocnews"
        ocnews-url "https://cloud.maralorn.de"
        ocnews-login "maralorn"
        ocnews-password "${(pkgs.privateValue {adminpass = "";} "nextcloud-admin").adminpass}"
      '';
    };
    software-updates = {
      feeds =
        [
          "http://packdeps.haskellers.com/feed/maralorn"
          "https://repology.org/maintainer/mail%40maralorn.de/feed-for-repo/nix_unstable/atom"
        ]
        # GitHub releases of things I need to manually update
        ++ map (name: "https://github.com/${name}/releases.atom") [
          "devture/email2matrix"
          "keyboardio/Chrysalis"
        ];
    };
  };
in {
  systemd.user = {
    services = {
      fiveetoolsmirror = {
        Unit.Description = "local 5etools mirror";
        Service = {
          ExecStart = "${pkgs.python3}/bin/python -m http.server --bind :: 5454 -d ${config.home.homeDirectory}/git/5etools
";
          Restart = "always";
        };
        Install.WantedBy = ["default.target"];
      };
      update-software-feeds = {
        Unit.Description = "Update software feeds";
        Service = {
          Type = "oneshot";
          ExecStart = toString (pkgs.writeShellScript "update-plans" ''
            ${commands.software-updates}/bin/software-updates -x reload
          '');
        };
      };
    };
    timers.update-software-feeds = {
      Unit.Description = "Update software feeds";
      Timer.OnCalendar = "hourly";
      Install.WantedBy = ["timers.target"];
    };
  };
  home = {
    packages = builtins.attrValues (commands // {inherit (pkgs) tut;});
  };
}
