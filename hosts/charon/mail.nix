{ config, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [ 25 587 443 993 ];

  users.users = {
    dovecot2.extraGroups = ["mail-cert"];
    postfix.extraGroups = ["mail-cert"];
  };

  users.groups = {
    mail-cert = {};
  };

  security.acme.certs = {
    "charon.olymp.space" = {
      email = "malte.brandy@maralorn.de";
      postRun = "systemctl restart postfix dovecot2";
      allowKeysForGroup = true;
      group = "mail-cert";
      extraDomains = {
        "maralorn.de" = null;
      };
    };
  };

  services = {
    # Mailserver
    rspamd.enable = true;
    rmilter = {
      enable = true;
      socketActivation = false;
      rspamd = {
        extraConfig = ''
          extended_spam_headers = true;
        '';
        enable = true;
      };
      postfix.enable = true;
    };
    dovecot2 = {
      enable = true;
      enablePop3 = false;
      modules = [ pkgs.dovecot_pigeonhole ];
      sslServerCert = "/var/lib/acme/charon.olymp.space/fullchain.pem";
      sslServerKey = "/var/lib/acme/charon.olymp.space/key.pem";
      extraConfig =
      ''
        postmaster_address=postmaster@charon.olymp.space

        ssl = required
        service auth {
          unix_listener /var/lib/postfix/queue/private/auth {
            mode = 0660
            user = postfix
            group = postfix
          }
        }
        protocol lda {
          mail_plugins = $mail_plugins sieve
        }
        plugin {
          sieve_extensions = +vnd.dovecot.duplicate
        }
      '';
      };
    postfix = {
      enable = true;
      enableSubmission = true;
      rootAlias = "maralorn";
      sslCert = "/var/lib/acme/charon.olymp.space/fullchain.pem";
      sslKey = "/var/lib/acme/charon.olymp.space/key.pem";
      extraAliases =
      ''
junge-erwachsene: :include:/etc/nixos/local/lists/junge-erwachsene
je-orga: :include:/etc/nixos/local/lists/je-orga
      '';
      lookupMX = true;
      extraConfig =
      ''
message_size_limit = 100000000
mailbox_size_limit = 100000000

mailbox_command = ${pkgs.dovecot}/libexec/dovecot/dovecot-lda -f "$SENDER" -a "$RECIPIENT" -d "$USER"

virtual_alias_domains = maralorn.de, choreutes.de, olymp.space, mathechor.de
smtp_bind_address6 = 2001:19f0:6c01:b0d::1
smtp_tls_security_level = may
smtpd_tls_security_level = may
      '';
      virtual =
      ''
junge-erwachsene@maralorn.de junge-erwachsene
je-orga@maralorn.de je-orga
@maralorn.de maralorn
@mathechor.de maralorn
@olymp.space maralorn
@choreutes.de choreutes
      '';
      submissionOptions = {
        milter_macro_daemon_name = "ORIGINATING";
        smtpd_tls_security_level = "encrypt";
        smtpd_sasl_type = "dovecot";
        smtpd_sasl_auth_enable = "yes";
        smtpd_sasl_path = "private/auth";
        smtpd_recipient_restrictions = "permit_sasl_authenticated,reject";
      };
    };
  };
}
