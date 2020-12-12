{ lib, pkgs, config, ... }: {
  systemd.user = {
    timers.logfeed = {
      Timer.OnCalendar = "19:55";
      Install.WantedBy = [ "timers.target" ];
    };
    services = {
      mail2rss = {
        Unit.Description = "Mail to rss exporter";
        Service = {
          ExecStart = pkgs.writeScript "mail2rss" "${pkgs.logfeed}/bin/mail2rss ${config.accounts.email.maildirBasePath} hera/Move/readlater > /var/www/rss/mails2.xml ";
          Type = "oneshot";
        };
      };
    };
  };
}
