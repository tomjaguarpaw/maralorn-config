{
  config,
  pkgs,
  lib,
  ...
}: let
  elementConfig = {
    default_server_config."m.homeserver" = {
      server_name = "maralorn.de";
      base_url = "https://matrix.maralorn.de";
    };
    integrations_ui_url = "";
    integgrations_rest_url = "";
    integrations_widgets_urls = [];
    roomDirectory.servers = ["matrix.org" "maralorn.de"];
    branding.welcomeBackgroundUrl = "https://cloud.maralorn.de/apps/theming/image/background";
  };
in {
  services.nginx = {
    enable = true;
    virtualHosts."element.maralorn.de" = {
      enableACME = true;
      forceSSL = true;
      root = pkgs.element-web.override (old: {conf = elementConfig;});
    };
  };
}
