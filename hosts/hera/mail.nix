{ config, ... }:
let
  certPath = "/var/lib/acme/hera.m-0.eu";
in
{
networking.firewall.allowedTCPPorts = [ 25 143 587 993 ];

services = {
  nginx = {
    enable = true;
    virtualHosts."hera.m-0.eu" = {
      enableACME = true;
    };
  };
};

containers.mail = {
  bindMounts = { "${certPath}" = { hostPath = certPath; }; };
  autoStart = true;
  config = { pkgs, lib, ... }: {
    imports = [../../system];
    mailserver = {
      enable = true;
      enableImapSsl = true;
      fqdn = "hera.m-0.eu";
      domains = [ "m-0.eu" "maralorn.de" "choreutes.de" "mathechor.de" ];
      loginAccounts = config.m-0.private.mailUsers;
      hierarchySeparator = "/";
      certificateScheme = 1;
      certificateFile = "${certPath}/fullchain.pem";
      keyFile = "${certPath}/key.pem";
      extraVirtualAliases = config.m-0.private.lists;
    };
  };
};

}
