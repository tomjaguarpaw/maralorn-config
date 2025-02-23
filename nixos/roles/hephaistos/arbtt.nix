{ config, ... }:
let
  port = 9999;
in
{
  services.nginx.virtualHosts.${config.m-0.virtualHosts.arbtt}.locations."/" = {
    proxyPass = "http://localhost:${toString port}";
  };
}
