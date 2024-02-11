{
  pkgs,
  config,
  lib,
  ...
}:
let
  certPath = "/var/lib/acme/hera.m-0.eu";
  nonMailboxDomains = [ "lists.maralorn.de" ];
  inherit (config.m-0) hosts virtualHosts;
  rspamd-address = "[::1]:11334";
in
{

  services = {
    nginx.virtualHosts.${virtualHosts.rspamd}.locations."/" = {
      proxyPass = "http://${rspamd-address}";
      proxyWebsockets = true;
    };
    prometheus.exporters = {
      postfix = {
        enable = true;
        systemd.enable = true;
        showqPath = "/var/lib/postfix/queue/public/showq";
        user = "postfix";
      };
    };
    rspamd = {
      workers = {
        controller = {
          bindSockets = [ rspamd-address ];
          extraConfig = ''
            secure_ip = "::1/128 127.0.0.1/32";
          '';
        };
        normal = { };
      };
      locals = {
        "multimap.conf".text =
          let
            allow-ip = builtins.toFile "allow-ip.map" "";
            allow-host = builtins.toFile "allow-host.map" ''
              ccc.de
              discourse.cloud
              haskell.org
              github.com
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
              score = -4;
              description = "Submitting IP listed in local allow list";
            }
            LOCAL_AL_HOST {
              type = "hostname"; # matches reverse DNS name of the host that performed message handoff
              filter = "tld" ; # matches eSLD (effective second level domain - a second-level domain or something that’s effectively so like example.com or example.za.org)
              score = -4;
              map = "${allow-host}";
              description = "Submitting host RDNS listed in local allow list";
            }
          '';
      };
    };
    postfix = {
      networks = [
        "[::1]/128"
        "127.0.0.1/32"
        "[${config.m-0.prefix}::]/64"
        "10.0.0.0/24"
      ];
      transport = "email2matrix.maralorn.de smtp:[::1]:2525";
      virtual = lib.mkForce (pkgs.privateValue "" "mail/virtual-regex");
      virtualMapType = "regexp";
      config = {
        # Allow TLSv1 because we need to be able to receive mail from legacy servers.
        smtpd_tls_protocols = lib.mkForce "TLSv1.3, TLSv1.2, TLSv1.1, TLSv1, !SSLv2, !SSLv3";
        virtual_mailbox_domains = lib.mkForce (
          builtins.toFile "vhosts" (
            lib.concatStringsSep "\n" (
              builtins.filter (x: !builtins.elem x nonMailboxDomains) config.mailserver.domains
            )
          )
        );
        smtp_bind_address = hosts.hera.A;
        smtp_bind_address6 = hosts.hera.AAAA;
      };
    };
  };
  mailserver = {
    dkimKeyDirectory = "/var/lib/opendkim/keys";
    enable = true;
    localDnsResolver = false; # Use unbound instead.
    openFirewall = true;
    enableImapSsl = true;
    enableManageSieve = true;
    fqdn = "hera.m-0.eu";
    rewriteMessageId = true;
    domains = [
      "m-0.eu"
      "maralorn.de"
      "choreutes.de"
      "lists.maralorn.de"
    ];
    loginAccounts = pkgs.privateValue { } "mail/users";
    hierarchySeparator = "/";
    certificateScheme = "manual";
    certificateFile = "${certPath}/fullchain.pem";
    keyFile = "${certPath}/key.pem";
    policydSPFExtraConfig = ''
      Mail_From_reject = False
      HELO_Whitelist = hosteurope.de
      skip_addresses = 127.0.0.0/8,::ffff:127.0.0.0/104,::1,130.83.0.0/16
    '';
  };
}
