{ config, lib, ... }:
let
  domain = config.m-0.virtualHosts."grist";
  clientId = "grist";
  port = 8484;
  authHost = config.services.kanidm.serverSettings.origin;
  scopes = [
    "email"
    "openid"
    "profile"
    "groups"
  ];
in
{
  virtualisation.oci-containers.containers.grist = {
    image = "gristlabs/grist-oss";
    volumes = [ "/var/lib/grist:/persist" ];
    environment = {
      GRIST_SESSION_SECRET = "";
      APP_HOME_URL = "https://${domain}";
      GRIST_SINGLE_ORG = "ott";
      GRIST_DEFAULT_EMAIL = "mail@maralorn.de";
      GRIST_OIDC_IDP_ISSUER = "${authHost}/oauth2/openid/${clientId}/.well-known/openid-configuration";
      GRIST_OIDC_IDP_CLIENT_ID = clientId;
      GRIST_OIDC_IDP_SKIP_END_SESSION_ENDPOINT = "true";
    };
    environmentFiles = [ config.age.secrets."grist-oauth2-client-secret".path ];
    extraOptions = [ "--network=host" ];
  };
  services = {
    kanidm.provision.systems.oauth2.${clientId} = {
      displayName = "Grist";
      originUrl = "https://${domain}/oauth2/callback";
      originLanding = "https://${domain}";
      scopeMaps.grist_users = scopes;
      enableLegacyCrypto = true; # Otherwise grist throws: OIDC callback failed: RPError: unexpected JWT alg received, expected RS256, got: ES256
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
    persistence.snapshoted.directories = [ "/var/lib/grist" ];
    persistence.unsnapshoted.directories = [ "/var/lib/docker" ];
  };

}
