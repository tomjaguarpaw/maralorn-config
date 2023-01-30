{
  pkgs,
  config,
  ...
}: {
  services.miniflux = {
    enable = true;
    adminCredentialsFile = pkgs.privatePath "miniflux-admin-credentials";
    config = {
      POLLING_FREQUENCY = "525600"; # We don‘t want polling so we set this to a year.
      BATCH_SIZE = "1000"; # To make sure that all feeds can get refreshed. Default is 100, which is probably fine.
      LISTEN_ADDR = "[${config.m-0.hosts.vpn.hera}]:8100";
    };
  };
  systemd.services."refresh-miniflux" = {
    script = "${pkgs.curl}/bin/curl -X PUT -H \"X-AUTH-TOKEN: $(cat $CREDENTIALS_DIRECTORY/miniflux_token)\" hera.vpn.m-0.eu:8100/v1/feeds/refresh";
    startAt = "20:00:00";
    serviceConfig.LoadCredential = ["miniflux_token:${pkgs.privatePath "miniflux-refresh-token"}"];
  };
}
