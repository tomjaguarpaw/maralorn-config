{ config, ... }:
let
  certPath = "/var/lib/acme/hera.m-0.eu";
in
{


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
      fqdn = "hera.m-0.eu";
      domains = [ "m-0.eu" "maralorn.de" "choreutes.de" "mathechor.de" ];
      loginAccounts = config.m-0.private.mailUsers;
      hierarchySeparator = "/";
      certificateScheme = 1;
      certificateFile = "${certPath}/fullchain.pem";
      keyFile = "${certPath}/key.pem";
      monitoring = {
        enable = true;
        alertAddress = "mail-alert@maralorn.de";
      };
      extraVirtualAliases = { "forward@maralorn.de" = ["maralorn@dermstadt.ccc.de" ]; }; # include junge-erwachsene hier.
    };
  };
};

}
