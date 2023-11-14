{
  lib,
  pkgs,
  config,
  ...
}:
let
  mail2rss =
    pkgs.writeHaskellScript
      {
        name = "mail2rss";
        bins = [
          pkgs.notmuch
          pkgs.mblaze
          pkgs.isync
          pkgs.rssfeeds
        ];
        imports = [ "System.Environment (setEnv)" ];
      }
      ''
        main = do
           setEnv "NOTMUCH_CONFIG" "${config.home.sessionVariables.NOTMUCH_CONFIG or ""}"
           mbsync "hera:Move/readlater,Archiv/unsortiert"
           notmuch "new" "--quiet"
           mail2rss "${config.accounts.email.maildirBasePath}" "hera/Move/readlater" &> Truncate "/var/www/rss/mails.xml"
           files <- notmuch "search" "--output" "files" "folder:hera/Move/readlater" |> capture
           writeOutput files |> mrefile "${config.accounts.email.maildirBasePath}/hera/Archiv/unsortiert"
           mbsync "hera:Move/readlater,Archiv/unsortiert"
           notmuch "new" "--quiet"
      '';
in
{
  systemd.user = {
    timers.mail2rss = {
      Timer.OnCalendar = "22:58";
      Install.WantedBy = [ "timers.target" ];
    };
    services = {
      mail2rss = {
        Unit.Description = "Mail to rss exporter";
        Service = {
          Environment = "PATH=${lib.makeBinPath [ pkgs.coreutils ]}";
          ExecStart = lib.getExe mail2rss;
          Type = "oneshot";
        };
      };
    };
  };
}
