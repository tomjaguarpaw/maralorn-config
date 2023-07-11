{
  pkgs,
  config,
  ...
}:
let
  inherit (config.m-0) virtualHosts;
in
{
  services.gitea = {
    package = pkgs.forgejo;
    enable = true;
    stateDir = "/var/lib/forgejo";
    settings = {
      actions.ENABLED = true;
      server = {
        ROOT_URL = "https://code.maralorn.de";
        HTTP_PORT = 3787;
      };
      service.DISABLE_REGISTRATION = true;
      ui = {
        DEFAULT_THEME = "codeberg-auto";
        THEMES = "forgejo-auto,forgejo-light,forgejo-dark,auto,gitea,arc-green,codeberg-auto,codeberg-light,codeberg-dark";
      };
    };
    appName = "Forgejo";
    database.type = "postgres";
  };
  services.nginx.virtualHosts.${virtualHosts."code"} = {
    forceSSL = true;
    enableACME = true;
    locations."/".proxyPass = "http://localhost:${
        toString config.services.gitea.settings.server.HTTP_PORT
      }";
    extraConfig = ''
      client_max_body_size 0;
    '';
  };
}
