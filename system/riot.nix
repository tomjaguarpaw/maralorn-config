{ config, pkgs, lib, ... }:

let
  riot_config = {
    default_hs_url = "https://matrix.maralorn.de";
    default_is_url = "https://vector.im";
    integrations_ui_url = "";
    integrations_rest_url = "";
    integrations_widgets_urls = [ ];
    bug_report_endpoint_url = "https://riot.im/bugreports/submit";
    welcomeUserId = "@riot-bot:matrix.org";
    piwik = false;
    features = {
      feature_lazyloading = "enable";
      feature_room_breadcrumbs = "enable";
    };
    roomDirectory = { servers = [ "matrix.org" "maralorn.de" ]; };
    branding = {
      welcomeBackgroundUrl =
        "https://cloud.maralorn.de/apps/theming/image/background";
    };
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
