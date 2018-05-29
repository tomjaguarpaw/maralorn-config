{ pkgs, ... }:
{
  systemd.user = {
    services.sort-mail =
    let
      sort-mail-script = pkgs.writeShellScriptBin "sort-mail" ''
        ${pkgs.isync}/bin/mbsync -a

        mv $HOME/data/aktuell/it/mail/.Move.kiva/cur/* $HOME/data/aktuell/it/mail-accounts/fb4/INBOX/new/
        mv $HOME/data/aktuell/it/mail/.Move.Auslandskoordination/cur/* $HOME/data/aktuell/it/mail-accounts/auslandskoordination/Malte/bearbeiten/new/

        mv $HOME/data/aktuell/it/mail-accounts/fb4/Move/privat/cur/* $HOME/data/aktuell/it/mail/new/
        mv $HOME/data/aktuell/it/mail-accounts/fb4/Move/Auslandskoordination/cur/* $HOME/data/aktuell/it/mail-accounts/auslandskoordination/Malte/bearbeiten/new/

        mv $HOME/data/aktuell/it/mail-accounts/auslandskoordination/Malte/privat/cur/* $HOME/data/aktuell/it/mail/new/
        mv $HOME/data/aktuell/it/mail-accounts/auslandskoordination/Malte/kiva/cur/* $HOME/data/aktuell/it/mail-accounts/fb4/INBOX/new/

        ${pkgs.isync}/bin/mbsync -a
      '';
    in {
      Unit = {
        Description = "Sort E-Mails";
      };

      Service = {
        Type = "oneshot";
        ExecStart="/bin/sh ${sort-mail-script}/bin/sort-mail";
      };
    };
    timers.sort-mail = {
      Timer = {
        OnCalendar = "minutely";
      };
    };
  };
}
