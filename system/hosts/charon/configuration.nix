{ config, pkgs, ... }:

let
   unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in
{
  system.stateVersion = "17.03";

  networking = {
    hostName = "charon.olymp.space";

    interfaces.ens3 = {
      ip4 = [ { address = "45.32.154.139"; prefixLength = 22; } ];
      ip6 = [ { address = "2001:19f0:6c01:b0d::1"; prefixLength = 64; } ];
    };
    defaultGateway = "45.32.152.1";
    nameservers = [ "108.61.10.10" "2001:19f0:300:1704::6" ];
  };

  environment.systemPackages = with pkgs; [
    pandoc
  ];

  imports = [
    ./hardware-configuration.nix
    ../../snippets/common.nix
    ../../snippets/init_ssh.nix
    /etc/nixos/local/config.nix
  ];

  boot.initrd.network.postCommands = "ip address add 2001:19f0:6c01:b0d::b007/64 dev eth0";
  boot.initrd.postMountCommands = "ip link set eth0 down";

  networking.firewall.allowedTCPPorts = [ 25 80 587 443 993 8448 22000 ];

  users.users = {
    choreutes = {
      description = "Tobias Schmalz";
      isNormalUser = true;
      passwordFile = "/etc/nixos/local/pw-choreutes";
    };
    swantje = {
      description = "Swantje Mahncke";
      isNormalUser = true;
      passwordFile = "/etc/nixos/local/pw-swantje";
    };
    dovecot2.extraGroups = ["certs"];
    postfix.extraGroups = ["certs"];
    matrix-synapse.extraGroups = ["certs"];
  };
  users.groups = {
    certs = {};
  };
  security.acme.certs = {
    "charon.olymp.space" = {
      email = "malte.brandy@maralorn.de";
      postRun = "systemctl restart postfix dovecot2";
      allowKeysForGroup = true;
      group = "certs";
      extraDomains = {
        "maralorn.de" = null;
      };
    };
    "matrix.maralorn.de" = {
      email = "malte.brandy@maralorn.de";
      postRun = "systemctl restart matrix-synapse";
      allowKeysForGroup = true;
      group = "certs";
      extraDomains = {
        "maralorn.de" = null;
      };
    };
  };
  services = {
    radicale = {
      enable = true;
      package = pkgs.radicale2;
      config = ''
        [auth]
        type = http_x_remote_user
      '';
    };
    nginx = {
      enable = true;
      virtualHosts."mathechor.de" = {
        serverAliases = ["www.mathechor.de"];
        forceSSL = true;
        enableACME = true;
        locations = {
          "~* Makefile".extraConfig = "deny all;";
          "/" = {
            root = "/var/www/mathechor/public";
            index = "index.html";
            extraConfig = "location ~* \.(otf)$ {add_header Access-Control-Allow-Origin *;}";
          };
        };
      };
      virtualHosts."intern.mathechor.de" = {
        forceSSL = true;
        enableACME = true;
        # See /etc/nixos/local/ für basic_auth pw.
        locations = {
          "~* Makefile".extraConfig = "deny all;";
          "/" = {
            root = "/var/www/mathechor/intern";
            index = "index.html";
          };
          "/mathechor.ics" = {
            proxyPass ="http://127.0.0.1:5232/maralorn/23e21619-29c6-17eb-043f-8ab5af00b46b/";
            extraConfig = ''
              proxy_set_header     X-Remote-User maralorn;
            '';
          };
        };
      };
      virtualHosts."dav.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        # See /etc/nixos/local/ für basic_auth pw.
        locations."/" = {
          proxyPass = "http://127.0.0.1:5232";
          extraConfig = "proxy_set_header     X-Remote-User $remote_user;";
        };
      };
      virtualHosts."blog.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "/var/www/blog/output";
            index = "index.html";
          };
        };
      };
      virtualHosts."charon.olymp.space" = {
        forceSSL = true;
        enableACME = true;
        default = true;
        locations = {
          "/ved.ics" = {
            proxyPass ="http://127.0.0.1:5232/maralorn/5a155c2c-1d87-e50d-874c-63f8858d1302/";
            extraConfig = ''
              proxy_set_header     X-Remote-User maralorn;
            '';
          };
        };
      };
      virtualHosts."matrix.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/_matrix" = {
            proxyPass = "http://[::1]:8008";
            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header X-Forwarded-For $remote_addr;
            '';
          };
        };
      };
    };

    # Postgres
    postgresql = {
      enable = true;
      package = pkgs.postgresql96;
    };

    # Synapse
    matrix-synapse = {
      enable = true;
      package = unstable.matrix-synapse;
      server_name = "maralorn.de";
      database_type = "psycopg2";
      max_upload_size = "30M";
      database_args = {
        user = "matrix-synapse";
        database = "matrix-synapse";
        cp_min = "5";
        cp_max = "10";
      };
      report_stats = true;
      tls_certificate_path = "/var/lib/acme/matrix.maralorn.de/fullchain.pem";
      tls_private_key_path = "/var/lib/acme/matrix.maralorn.de/key.pem";
      listeners = [
        {
          port = 8448;
          bind_address = "::";
          resources = [ { compress = true; names = [ "client" ]; } { compress = false; names = [ "federation" ]; } ];
          x_forwarded = false;
        }
        {
          port = 8008;
          bind_address = "::1";
          resources = [ { compress = false; names = [ "client" ]; } { compress = false; names = [ "federation" ]; } ];
          x_forwarded = true;
          tls = false;
        }
      ];
    };

    # Taskserver
    # taskserver = {
    #   enable = true;
    #   fqdn = config.networking.hostName;
    #   listenHost = "::";
    #   organisations.users.users = [ "maralorn" ];
    #   config = { request.limit = 0; };
    # };

    # Mailserver
    rspamd.enable = true;
    rmilter = {
      enable = true;
      socketActivation = false;
      rspamd = {
        extraConfig = ''
          extended_spam_headers = yes;
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
message_size_limit = 30720000

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

  boot.loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/vda";
  };

}
