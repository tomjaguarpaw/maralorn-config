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
  environment.persistence.snapshoted.directories = [
    "/var/www/rss"
    "/var/lib/postgresql"
  ];
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
    nginx.virtualHosts.${virtualHosts.rss}.locations = {
      "/" = {
        proxyPass = "http://${address}";
        proxyWebsockets = true;
      };
      "/own/".alias = "/var/www/rss/";
    };
  };
  systemd.services.refresh-miniflux = {
    script = ''
      ${lib.getExe pkgs.curl} -X PUT -H @$CREDENTIALS_DIRECTORY/auth-header https://${virtualHosts.rss}/v1/feeds/refresh
      ${lib.getExe pkgs.curl} -X PUT -H @$CREDENTIALS_DIRECTORY/auth-header-watchfeeds https://${virtualHosts.rss}/v1/feeds/refresh
      ${lib.getExe pkgs.curl} -X PUT -H @$CREDENTIALS_DIRECTORY/auth-header-softwareupdates https://${virtualHosts.rss}/v1/feeds/refresh
    '';
    startAt = "19:00";
    serviceConfig = {
      Type = "oneshot";
      LoadCredential = [
        "auth-header:${config.age.secrets.miniflux-refresh-auth-header.path}"
        "auth-header-watchfeeds:${config.age.secrets.miniflux-refresh-auth-header-watchfeeds.path}"
        "auth-header-softwareupdates:${config.age.secrets.miniflux-refresh-auth-header-softwareupdates.path}"
      ];
    };
  };
}
