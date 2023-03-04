{
  pkgs,
  config,
  ...
}: let
  inherit (config.m-0) hosts;
  address = "[::1]:8100";
in {
  services = {
    miniflux = {
      enable = true;
      adminCredentialsFile = config.age.secrets.miniflux-admin-credentials.path;
      config = {
        POLLING_FREQUENCY = "525600"; # We don‘t want polling so we set this to a year.
        BATCH_SIZE = "1000"; # To make sure that all feeds can get refreshed. Default is 100, which is probably fine.
        LISTEN_ADDR = address;
      };
    };
    nginx.virtualHosts."rss.vpn.m-0.eu" = {
      locations."/" = {
        proxyPass = "http://${address}";
        proxyWebsockets = true;
      };
    };
  };
  systemd.services = {
    rss-server = {
      serviceConfig.ExecStart = "${pkgs.python3}/bin/python -m http.server --bind ${hosts.tailscale.hera.AAAA} 8842 -d /var/www/rss";
      wantedBy = ["multi-user.target"];
    };
    mastodon-digest = {
      script = ''
        now=$(date "+%Y-%m-%d")
        mkdir -p /var/www/rss/mastodon/$now-home-feed-highlights
        mkdir -p /var/www/rss/mastodon/$now-read-all-list
        mkdir -p /var/www/rss/mastodon/$now-tags
        set -o allexport
        source $CREDENTIALS_DIRECTORY/mastodon-auth-env
        set +o allexport
        ${pkgs.mastodon_digest}/bin/mastodon_digest -o /var/www/rss/mastodon/$now-home-feed-highlights -n 24 -t normal
        ${pkgs.mastodon_digest}/bin/mastodon_digest -o /var/www/rss/mastodon/$now-read-all-list -n 24 -t all --theme no-boosts -f list:3811
        ${pkgs.mastodon_digest}/bin/mastodon_digest -o /var/www/rss/mastodon/$now-tags -n 24 -t all -f list:4160
        ${pkgs.logfeed}/bin/mastodon2rss /var/www/rss/mastodon.xml /var/www/rss/mastodon
      '';
      serviceConfig = {
        Type = "oneshot";
        LoadCredential = ["mastodon-auth-env:${config.age.secrets.mastodon-auth-env.path}"];
      };
    };
    refresh-miniflux = {
      script = "${pkgs.curl}/bin/curl -X PUT -H @$CREDENTIALS_DIRECTORY/auth-header rss.vpn.m-0.eu/v1/feeds/refresh";
      after = ["mastodon-digest.service"];
      requires = ["mastodon-digest.service"];
      startAt = "9:00:00";
      serviceConfig = {
        Type = "oneshot";
        LoadCredential = ["auth-header:${config.age.secrets.miniflux-refresh-auth-header.path}"];
      };
    };
  };
}
