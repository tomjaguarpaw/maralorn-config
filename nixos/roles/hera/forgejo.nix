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
      actions = {
        DEFAULT_ACTIONS_URL = "https://github.com";
        ENABLED = true;
      };
      metrics.ENABLED = true;
      server = {
        ROOT_URL = "https://code.maralorn.de";
        HTTP_PORT = 3787; # Default port was blocked
      };
      service.DISABLE_REGISTRATION = true;
      ui.DEFAULT_THEME = "gitea-auto";
      mailer = {
        ENABLED = true;
        FROM = "forgejo@code.maralorn.de"; # Could also be "Forgejo <...>" or "Git minion on Hera"
        USER = "forgejo@code.maralorn.de";
        PROTOCOL = "smtps";
        SMTP_ADDR = "hera.m-0.eu";
        SMTP_PORT = "465";
      };
      session.SESSION_LIFE_TIME = 2419200; # 2 weeks
      # Required for 1.20 compat delete on 23.11
      packages.CHUNKED_UPLOAD_PATH = "${config.services.gitea.stateDir}/tmp/package-upload";
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
    locations."/".proxyPass = "http://localhost:${toString config.services.gitea.settings.server.HTTP_PORT}";
    extraConfig = ''
      client_max_body_size 0;
    '';
  };
}
