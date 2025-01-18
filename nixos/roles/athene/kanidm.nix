{ lib, config, ... }:
let
  domain = config.m-0.virtualHosts."athene.id";
  certDir = config.security.acme.certs.${domain}.directory;
in
{
  services = {
    kanidm = {
      enableServer = true;
      enableClient = true;
      clientSettings.uri = "https://${domain}";
      provision = {
        enable = true;
        persons.maralorn = {
          mailAddresses = [ "mail@maralorn.de" ];
          displayName = "maralorn";
        };
        groups.outline_users.members = [ "maralorn" ];
        groups.grist_users.members = [ "maralorn" ];
      };
      serverSettings = {
        origin = "https://${domain}";
        domain = "id.maralorn.de";
        tls_chain = "${certDir}/fullchain.pem";
        tls_key = "${certDir}/key.pem";
      };
    };
    nginx.virtualHosts.${domain}.locations."/".proxyPass = "https://127.0.0.1:8443";
  };
  environment = lib.mkIf config.has-persistence {
    persistence.snapshoted.directories = [ "/var/lib/kanidm" ];
  };
  systemd.services.kanidm.serviceConfig.SupplementaryGroups = [ "nginx" ];
}
