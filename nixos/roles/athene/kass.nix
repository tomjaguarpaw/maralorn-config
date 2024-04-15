{
  config,
  lib,
  pkgs,
  ...
}:
{
  systemd.services.kass = {
    requires = [ "couchdb.service" ];
    after = [ "couchdb.service" ];
    serviceConfig = {
      DynamicUser = true;
      ExecStart = "${lib.getExe pkgs.kass} web";
    };
    wantedBy = [ "multi-user.target" ];
  };
  services.nginx.virtualHosts.${config.m-0.virtualHosts.kass}.locations."/" = {
    proxyPass = "http://localhost:5344";
    proxyWebsockets = true;
  };
}
