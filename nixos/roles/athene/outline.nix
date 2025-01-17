{ config, lib, ... }:
let
  domain = config.m-0.virtualHosts."notes";
  clientId = "outline";
  port = 3796;
  scopes = [
    "email"
    "openid"
    "profile"
    "groups"
  ];
in
{
  age.secrets."outline-oauth2-client-secret".owner = config.services.outline.user;
  services = {
    kanidm.provision.systems.oauth2.${clientId} = {
      displayName = "Notes";
      allowInsecureClientDisablePkce = true;
      originUrl = "https://${domain}/auth/oidc.callback";
      originLanding = "https://${domain}";
      scopeMaps.outline_users = scopes;
    };
    outline = {
      enable = true;
      forceHttps = false;
      storage.storageType = "local";
      publicUrl = "https://${domain}";
      inherit port;
      oidcAuthentication =
        let
          authHost = config.services.kanidm.serverSettings.origin;
        in
        {
          authUrl = "${authHost}/ui/oauth2";
          inherit clientId;
          clientSecretFile = config.age.secrets."outline-oauth2-client-secret".path;
          displayName = "id.maralorn.de";
          tokenUrl = "${authHost}/oauth2/token";
          userinfoUrl = "${authHost}/oauth2/openid/outline/userinfo";
        };
    };
    nginx.virtualHosts.${domain}.locations."/" = {
      proxyPass = "http://localhost:${toString port}";
      proxyWebsockets = true;
      # Extra config copied from https://docs.getoutline.com/s/hosting/doc/nginx-6htaRboR57
      # It is unclear what of it helps but something does
      extraConfig = ''
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Scheme $scheme;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_redirect off;
      '';
    };
  };
  environment = lib.mkIf config.has-persistence {
    persistence.snapshoted.directories = [ "/var/lib/outline" ];
  };
}
