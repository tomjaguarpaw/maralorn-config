{ config, ... }:
let certPath = "/var/lib/acme/hera.m-0.eu";
in {
  networking.firewall = { allowedTCPPorts = [ 25 143 587 993 ]; };

  m-0.monitoring = [
    {
      name = "mail-server";
      host = "hera-intern:9101";
    }
    {
      name = "postfix";
      host = "hera-intern:9154";
    }
    {
      name = "dovecot";
      host = "hera-intern:9166";
    }
  ];

  containers.mail = {
    bindMounts = {
      "${certPath}" = { hostPath = certPath; };
      "/var/www/rss" = {
        hostPath = "/var/www/rss";
        isReadOnly = false;
      };
    };
    autoStart = true;
    config = { pkgs, lib, ... }: {
      imports =
        [ ../../system "${(import ../../nix/sources.nix).nixos-mailserver}" ];
      services.prometheus.exporters = {
        node.port = 9101;
        postfix = {
          enable = true;
          systemd.enable = true;
          showqPath = "/var/lib/postfix/queue/public/showq";
          user = "postfix";
        };
      };
      systemd.services = {
        atomail = {
          script = let
            atomail = pkgs.fetchFromGitHub {
              owner = "remko";
              repo = "atomail";
              rev = "f079966cb808f51fcc67be91b609942cdb49898a";
              sha256 = "0a4j4xajn2yysgcb17jmb6ak148kk0kwf7khml7dbnh7807fv9b6";
            };
          in ''
            ${pkgs.python}/bin/python ${atomail}/atomail.py --title "Readlater-E-Mails" --uri="http://localhost:8842/mails.xml" /var/www/rss/mails.xml --mode=maildir --file "/var/vmail/maralorn.de/malte.brandy/.Move.readlater/" --max-items=500 --max-time=2880
            ${pkgs.rsync}/bin/rsync -a /var/vmail/maralorn.de/malte.brandy/.Move.readlater/cur/ /var/vmail/maralorn.de/malte.brandy/.Archiv.unsortiert/cur --remove-source-files
          '';
          startAt = "19:58:00";
          serviceConfig.Type = "oneshot";
        };
      };
      services.postfix = {
        networks = [ "[${config.m-0.prefix}::]/64" "10.0.0.0/24" ];
        transport = "email2matrix.maralorn.de smtp:[::1]:2525";
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
        extraVirtualAliases = config.m-0.private.lists // {
          "weather-channel@maralorn.de" =
            [ "weather@email2matrix.maralorn.de" ];
          "subjects-channel@maralorn.de" =
            [ "subjects@email2matrix.maralorn.de" ];
          "notify-channel@maralorn.de" = [ "notify@email2matrix.maralorn.de" ];
          "monitoring-channel@maralorn.de" =
            [ "monitoring@email2matrix.maralorn.de" ];
        };
        policydSPFExtraConfig = ''
          Mail_From_reject = False
          HELO_Whitelist = hosteurope.de
          skip_addresses = 127.0.0.0/8,::ffff:127.0.0.0/104,::1,130.83.0.0/16
        '';
      };
    };
  };
}
