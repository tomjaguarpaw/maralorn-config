{ pkgs, config, ... }:
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
      mailer = {
        ENABLED = true;
        FROM = "forgejo@code.maralorn.de";
        USER = "forgejo@code.maralorn.de";
        PROTOCOL = "smtps";
        SMTP_ADDR = "hera.m-0.eu";
        SMTP_PORT = "465";
      };
    };
    mailerPasswordFile = "/run/credentials/gitea.service/forgejo-mail-password";
    appName = "Forgejo";
    database.type = "postgres";
  };
  systemd.services.gitea.serviceConfig.LoadCredential = [
    "forgejo-mail-password:${config.age.secrets.forgejo-mail-password.path}"
  ];
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
