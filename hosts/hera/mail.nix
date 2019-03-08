{ config, ... }:
let
  certPath = "/var/lib/acme/hera.m-0.eu";
  inherit (config.services.prometheus.exporters.node) firewallFilter;
in
{
networking.firewall = {
  allowedTCPPorts = [ 25 143 587 993 ];
  extraCommands = ''
    ip6tables -A nixos-fw  -s ${config.m-0.prefix}::/64 -p tcp -m tcp --dport 9101 -j nixos-fw-accept
    ip6tables -A nixos-fw  -s ${config.m-0.prefix}::/64 -p tcp -m tcp --dport 9154 -j nixos-fw-accept
    ip6tables -A nixos-fw  -s ${config.m-0.prefix}::/64 -p tcp -m tcp --dport 9166 -j nixos-fw-accept
  '';
};

m-0.monitoring = [
  { name = "mail-server"; host = "hera-intern:9101"; }
  { name = "postfix"; host = "hera-intern:9154"; }
  { name = "dovecot"; host = "hera-intern:9166"; }
];

containers.mail = {
  bindMounts = { "${certPath}" = { hostPath = certPath; }; };
  autoStart = true;
  config = { pkgs, lib, ... }: {
    imports = [../../system];
    services.prometheus.exporters = {
      node.port = 9101;
      postfix = {
        enable = true;
        openFirewall = true;
        inherit firewallFilter;
        systemd.enable = true;
      };
      dovecot = {
        enable = true;
        openFirewall = true;
        inherit firewallFilter;
      };

    };
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
      policydSPFExtraConfig = ''
        Mail_From_reject = False
        HELO_Whitelist = hosteurope.de
        skip_addresses = 127.0.0.0/8,::ffff:127.0.0.0/104,::1,130.83.0.0/16
      '';
    };
  };
};

}
