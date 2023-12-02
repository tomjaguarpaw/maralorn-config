{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (config.m-0) virtualHosts;
  address = "[::1]:8100";
in
{
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
    nginx.virtualHosts.${virtualHosts.rss} = {
      locations = {
        "/" = {
          proxyPass = "http://${address}";
          proxyWebsockets = true;
        };
        "/own/".alias = "/var/www/rss/";
      };
    };
  };
  systemd.services = {
    mastodon-digest = {
      script = ''
        now=$(date "+%Y-%m-%d")
        mkdir -p /var/www/rss/mastodon/$now-home-feed-highlights
        mkdir -p /var/www/rss/mastodon/$now-read-all-list
        mkdir -p /var/www/rss/mastodon/$now-tags
        set -o allexport
        source $CREDENTIALS_DIRECTORY/mastodon-auth-env
        set +o allexport
        ${lib.getExe pkgs.mastodon_digest} -o /var/www/rss/mastodon/$now-home-feed-highlights -n 24 -t normal
        ${lib.getExe pkgs.mastodon_digest} -o /var/www/rss/mastodon/$now-read-all-list -n 24 -t all --theme no-boosts -f list:3811
        ${lib.getExe pkgs.mastodon_digest} -o /var/www/rss/mastodon/$now-tags -n 24 -t all -f list:4160
        ${lib.getBin pkgs.rssfeeds}/bin/mastodon2rss /var/www/rss/mastodon.xml /var/www/rss/mastodon
      '';
      serviceConfig = {
        Type = "oneshot";
        LoadCredential = ["mastodon-auth-env:${config.age.secrets.mastodon-auth-env.path}"];
      };
    };
    refresh-miniflux = {
      script = ''
        ${lib.getExe pkgs.curl} -X PUT -H @$CREDENTIALS_DIRECTORY/auth-header https://${virtualHosts.rss}/v1/feeds/refresh
        ${lib.getExe pkgs.curl} -X PUT -H @$CREDENTIALS_DIRECTORY/auth-header-watchfeeds https://${virtualHosts.rss}/v1/feeds/refresh
        ${lib.getExe pkgs.curl} -X PUT -H @$CREDENTIALS_DIRECTORY/auth-header-softwareupdates https://${virtualHosts.rss}/v1/feeds/refresh
      '';
      after = ["mastodon-digest.service"];
      requires = ["mastodon-digest.service"];
      startAt = "23:00";
      serviceConfig = {
        Type = "oneshot";
        LoadCredential = [
          "auth-header:${config.age.secrets.miniflux-refresh-auth-header.path}"
          "auth-header-watchfeeds:${config.age.secrets.miniflux-refresh-auth-header-watchfeeds.path}"
          "auth-header-softwareupdates:${config.age.secrets.miniflux-refresh-auth-header-softwareupdates.path}"
        ];
      };
    };
  };
}
