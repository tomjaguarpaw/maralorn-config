{ config, ... }:
{
  services.rss-bridge = {
    enable = true;
    virtualHost = config.m-0.virtualHosts."rss-bridge";
    config = {
      system.enabled_bridges = [ "*" ];
      FileCache.path = "${config.services.rss-bridge.dataDir}/cache/";
    };
  };
  environment.persistence.snapshoted.directories = [ config.services.rss-bridge.dataDir ];
}
