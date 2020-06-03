{ config, pkgs, lib, ... }:

let
  riot_config = {
    default_server_config."m.homeserver" = {
      server_name = "maralorn.de";
      base_url = null;
    };
    roomDirectory.servers = [ "matrix.org" "maralorn.de" ];
    branding.welcomeBackgroundUrl =
      "https://cloud.maralorn.de/apps/theming/image/background";
  };
in {
  services.nginx = {
    enable = true;
    virtualHosts."riot.maralorn.de" = {
      enableACME = true;
      forceSSL = true;
      root = pkgs.riot-web.override (old: { conf = riot_config; });
    };
  };

}
