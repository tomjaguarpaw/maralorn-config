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
    iptables -A nixos-fw  -s 10.0.0.0/24 -p tcp -m tcp --dport 8842 -j nixos-fw-accept
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
    imports = [
      ../../system
      "${(builtins.fetchGit "ssh://git@hera/nixos-mailserver")}"
    ];
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
    systemd.services = {
      atomail = {
        script = ''
          ${pkgs.python}/bin/python ${builtins.fetchGit "https://github.com/remko/atomail.git"}/atomail.py --title "Readlater-E-Mails" --uri="http://hera-intern-v4:8842/rss.xml" /var/www/rss.xml --mode=maildir --file "/var/vmail/maralorn.de/malte.brandy/.Move.readlater/" --max-items=100
          ${pkgs.rsync}/bin/rsync -a /var/vmail/maralorn.de/malte.brandy/.Move.readlater/cur/ /var/vmail/maralorn.de/malte.brandy/.Archiv.unsortiert/cur --remove-source-files
        '';
        startAt = "19:58:00";
        serviceConfig.Type = "oneshot";
      };
      rss-server = {
        preStart = "mkdir -p /var/www";
        script = ''
          cd /var/www
          ${pkgs.haskellPackages.hserv}/bin/hserv -p8842
        '';
        wantedBy = [ "multi-user.target" ];
      };
    };
    services.postfix.networks = [ "[${config.m-0.prefix}::]/64" "10.0.0.0/24" ];
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
