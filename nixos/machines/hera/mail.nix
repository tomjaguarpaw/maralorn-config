{ pkgs, config, lib, ... }:
let
  certPath = "/var/lib/acme/hera.m-0.eu";
in
{
  networking.firewall.allowedTCPPorts = [ 25 143 587 993 ];

  m-0.monitoring = [
    {
      name = "postfix on hera";
      host = "hera-intern:9154";
    }
  ];

  imports =
    [ ../../roles "${(import ../../../nix/sources.nix).nixos-mailserver}" ];
  systemd.services = {
    rspamd = {
      serviceConfig = {
        Restart = "always";
        RestartSec = 3;
      };
      unitConfig = {
        StartLimitIntervalSec = 60;
        StartLimitBurst = 15;
      };
    };
  };
  services = {
    prometheus.exporters = {
      postfix = {
        enable = true;
        systemd.enable = true;
        showqPath = "/var/lib/postfix/queue/public/showq";
        user = "postfix";
      };
    };
    postfix = {
      networks = [ "[${config.m-0.prefix}::]/64" "10.0.0.0/24" ];
      transport = "email2matrix.maralorn.de smtp:[::1]:2525";
      config = {
        # Allow TLSv1 because we need to be able to receive mail from legacy servers.
        smtpd_tls_protocols = lib.mkForce
          "TLSv1.3, TLSv1.2, TLSv1.1, TLSv1, !SSLv2, !SSLv3";
      };
    };
    opendkim.keyPath = "/var/dkim";
  };
  mailserver = {
    enable = true;
    enableImapSsl = true;
    fqdn = "hera.m-0.eu";
    domains = [ "m-0.eu" "maralorn.de" "choreutes.de" "mathechor.de" ];
    forwards = pkgs.privateValue {} "mail/forwards";
    loginAccounts = pkgs.privateValue {} "mail/users";
    hierarchySeparator = "/";
    certificateScheme = 1;
    certificateFile = "${certPath}/fullchain.pem";
    keyFile = "${certPath}/key.pem";
    policydSPFExtraConfig = ''
      Mail_From_reject = False
      HELO_Whitelist = hosteurope.de
      skip_addresses = 127.0.0.0/8,::ffff:127.0.0.0/104,::1,130.83.0.0/16
    '';
  };
}
