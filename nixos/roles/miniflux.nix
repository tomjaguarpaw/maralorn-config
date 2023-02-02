{
  pkgs,
  config,
  ...
}: let
  inherit (config.m-0) hosts;
in {
  services.miniflux = {
    enable = true;
    adminCredentialsFile = config.age.secrets.miniflux-admin-credentials.path;
    config = {
      POLLING_FREQUENCY = "525600"; # We don‘t want polling so we set this to a year.
      BATCH_SIZE = "1000"; # To make sure that all feeds can get refreshed. Default is 100, which is probably fine.
      LISTEN_ADDR = "[${hosts.vpn.hera}]:8100";
    };
  };
  systemd.services = {
    rss-server = {
      serviceConfig.ExecStart = "${pkgs.python3}/bin/python -m http.server --bind ${hosts.vpn.hera} 8842 -d /var/www/rss";
      wantedBy = ["multi-user.target"];
    };
    mastodon-digest = {
      script = ''
        now=$(date "+%Y-%m-%d")
        mkdir -p /var/www/rss/mastodon/$now-home-feed-highlights
        mkdir -p /var/www/rss/mastodon/$now-read-all-list
        source $CREDENTIALS_DIRECTORY/mastodon-auth-env
        ${pkgs.mastodon_digest}/bin/mastodon_digest -o /var/www/rss/mastodon/$now-home-feed-highlights -n 24 -t lax --theme light
        ${pkgs.mastodon_digest}/bin/mastodon_digest -o /var/www/rss/mastodon/$now-read-all-list -n 24 -t all --theme light -f list:3811
        ${pkgs.logfeed}/bin/mastodon2rss /var/www/rss/mastodon.xml /var/www/rss/mastodon
      '';
      serviceConfig = {
        Type = "oneshot";
        LoadCredential = ["mastodon-auth-env:${config.age.secrets.mastodon-auth-env.path}"];
      };
    };
    refresh-miniflux = {
      script = "${pkgs.curl}/bin/curl -X PUT -H @$CREDENTIALS_DIRECTORY/auth-header hera.vpn.m-0.eu:8100/v1/feeds/refresh";
      after = ["mastodon-digest.service"];
      requires = ["mastodon-digest.service"];
      startAt = "20:00:00";
      serviceConfig = {
        Type = "oneshot";
        LoadCredential = ["auth-header:${config.age.secrets.miniflux-refresh-auth-header.path}"];
      };
    };
  };
}
