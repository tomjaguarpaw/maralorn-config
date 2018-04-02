{ pkgs, ... }:
{
  services = {
    radicale = {
      enable = true;
      package = pkgs.radicale2;
      config = ''
        [auth]
        type = http_x_remote_user
      '';
    };
    nginx = {
      virtualHosts."dav.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        # See /etc/nixos/local/ für basic_auth pw.
        locations."/" = {
          proxyPass = "http://127.0.0.1:5232";
          extraConfig = "proxy_set_header     X-Remote-User $remote_user;";
        };
      };
    };
  };
}
