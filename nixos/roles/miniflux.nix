{
  pkgs,
  config,
  ...
}: {
  services.miniflux = {
    enable = true;
    adminCredentialsFile = pkgs.privatePath "miniflux-admin-credentials";
    config = {
      DEBUG = true;
      LOG_DATE_TIME = true;
      SCHEDULER_SERVICE = false;
      BATCH_SIZE = 1000;
      LISTEN_ADDRESS = "${config.m-0.hosts.vpn.hera}:8100";
    };
  };
}
