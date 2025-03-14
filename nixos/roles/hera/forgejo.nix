{ config, ... }:
let
  inherit (config.m-0) virtualHosts;
in
{

  services.forgejo = {
    enable = true;
    database.type = "postgres";
    secrets.mailer.PASSWD = config.age.secrets.forgejo-mail-password.path;
    settings = {
      actions = {
        DEFAULT_ACTIONS_URL = "https://github.com";
        ENABLED = true;
      };
      metrics.ENABLED = true;
      server = {
        ROOT_URL = "https://code.maralorn.de";
        HTTP_PORT = 3787; # Default port was blocked
      };
      service = {
        DISABLE_REGISTRATION = true;
        ENABLE_NOTIFY_MAIL = true;
      };
      ui.DEFAULT_THEME = "gitea-auto";
      admin.DEFAULT_EMAIL_NOTIFICATIONS = "enabled";
      mailer = {
        ENABLED = true;
        FROM = "forgejo@code.maralorn.de"; # Could also be "Forgejo <...>" or "Git minion on Hera"
        USER = "forgejo@code.maralorn.de";
        PROTOCOL = "smtps";
        SMTP_ADDR = "hera.m-0.eu";
        SMTP_PORT = "465";
      };
      session.SESSION_LIFE_TIME = 2419200; # 2 weeks
    };
  };
  services.nginx.virtualHosts.${virtualHosts."code"} = {
    locations."/".proxyPass =
      "http://localhost:${toString config.services.forgejo.settings.server.HTTP_PORT}";
    extraConfig = ''
      client_max_body_size 0;
    '';
  };
}
