{
  pkgs,
  config,
  ...
}: {
  services.miniflux = {
    enable = true;
    adminCredentialsFile = pkgs.privatePath "miniflux-admin-credentials";
    config = {
      DEBUG = "1";
      SCHEDULER_SERVICE = "0";
      BATCH_SIZE = "1000";
      LISTEN_ADDR = "[${config.m-0.hosts.vpn.hera}]:8100";
    };
  };
}
