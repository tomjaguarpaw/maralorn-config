{ pkgs, config, ... }:
let inherit (config.m-0) virtualHosts;
in {
  services.gitea = {
    package = pkgs.forgejo;
    enable = true;
    stateDir = "/var/lib/forgejo";
    settings.server = {
      ROOT_URL = "http://code.maralorn.de";
      HTTP_PORT = 3787;
    };
    appName = "maralorns code";
  };
  services.nginx.virtualHosts.${virtualHosts."code"} = {
    locations."/".proxyPass = "http://localhost:${
        toString config.services.gitea.settings.server.HTTP_PORT
      }";
    extraConfig = ''
      client_max_body_size 0;
    '';

  };
}
