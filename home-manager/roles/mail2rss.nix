{ lib, pkgs, config, ... }: {
  systemd.user = {
    timers.mail2rss = {
      Timer.OnCalendar = "19:57";
      Install.WantedBy = [ "timers.target" ];
    };
    services = {
      mail2rss = {
        Unit.Description = "Mail to rss exporter";
        Service = {
          ExecStart = toString (pkgs.writeShellScript "mail2rss"
            "${pkgs.logfeed}/bin/mail2rss ${config.accounts.email.maildirBasePath} hera/Move/readlater > /var/www/rss/mails2.xml");
          Type = "oneshot";
        };
      };
    };
  };
}
