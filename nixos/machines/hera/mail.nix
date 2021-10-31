{ pkgs, config, lib, ... }:
let
  certPath = "/var/lib/acme/hera.m-0.eu";
  nonMailboxDomains = [ "lists.maralorn.de" ];
in
{
  m-0.monitoring = [
    {
      name = "postfix on hera";
      host = "hera-intern:9154";
    }
  ];

  imports = [ ../../roles "${(import ../../../nix/sources.nix).nixos-mailserver}" ];

  services = {
    prometheus.exporters = {
      postfix = {
        enable = true;
        systemd.enable = true;
        showqPath = "/var/lib/postfix/queue/public/showq";
        user = "postfix";
      };
    };
    rspamd.locals = {
      "multimap.conf".text =
        let
          allow-ip = builtins.toFile "allow-ip.map" ''
          '';
          allow-host = builtins.toFile "allow-host.map" ''
            gmx.de
            web.de
            google.com
            meteosafe.com # Wetterwarnungen
            # tu-darmstadt.de # Not safe because of spam via cda
            ccc.de
            discourse.cloud
            haskell.org
            github.com
            vocalensemble-darmstadt.de
            sit-mainz.info # Bistum Mainz
          '';
        in
        ''
          # local.d/multimap.conf
          # allow lists
          LOCAL_AL_IP {
            type = "ip"; # matches IP of the host that performed message handoff (against radix map)
            map = "${allow-ip}";
            group = "local_al_ip";
            score = -15;
            description = "Submitting IP listed in local allow list";
          }
          LOCAL_AL_HOST {
            type = "hostname"; # matches reverse DNS name of the host that performed message handoff
            filter = "tld" ; # matches eSLD (effective second level domain - a second-level domain or something that’s effectively so like example.com or example.za.org)
            score = -15;
            map = "${allow-host}";
            description = "Submitting host RDNS listed in local allow list";
          }
        '';
    };
    postfix = {
      networks = [ "[::1]/128" "127.0.0.1/32" "[${config.m-0.prefix}::]/64" "10.0.0.0/24" ];
      transport = "email2matrix.maralorn.de smtp:[::1]:2525";
      config = {
        # Allow TLSv1 because we need to be able to receive mail from legacy servers.
        smtpd_tls_protocols = lib.mkForce "TLSv1.3, TLSv1.2, TLSv1.1, TLSv1, !SSLv2, !SSLv3";
        virtual_mailbox_domains = lib.mkForce (builtins.toFile "vhosts" (lib.concatStringsSep "\n" (builtins.filter (x: !builtins.elem x nonMailboxDomains) config.mailserver.domains)));
      };
    };
  };
  mailserver = {
    dkimKeyDirectory = "/var/lib/opendkim/keys";
    enable = true;
    openFirewall = true;
    enableImapSsl = true;
    enableManageSieve = true;
    fqdn = "hera.m-0.eu";
    rewriteMessageId = true;
    domains = [ "m-0.eu" "maralorn.de" "choreutes.de" "mathechor.de" "lists.maralorn.de" ];
    forwards = pkgs.privateValue { } "mail/forwards";
    loginAccounts = pkgs.privateValue { } "mail/users";
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
