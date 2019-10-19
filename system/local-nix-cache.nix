{ config, pkgs, lib, ... }:
let inherit (import ../lib) sources;
  local-nix-cache = import sources.local-nix-cache { };
in {
  imports = [ (local-nix-cache.path + "/module.nix") ];

  local-nix-cache = {
    server.enable = true;
    client.enable = true;
  };
  networking.firewall.allowedTCPPorts = [ config.local-nix-cache.server.port ];

  systemd.services.local-nix-cache.path = [ local-nix-cache.nix ];
  systemd.services.local-nix-cache.serviceConfig.Restart = "always";

  services.avahi.enable = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;

}
