{ pkgs, ... }:
let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in {
  systemd.user = {
    services.sort-mail =
    let
      sort-mail-script = pkgs.writeShellScriptBin "sort-mail" ''
        ${unstable.isync}/bin/mbsync -a
        mv $HOME/data/aktuell/it/mail/.Move.kiva/cur/* $HOME/data/aktuell/it/mail-accounts/fb4/INBOX/new/
        mv $HOME/data/aktuell/it/mail-accounts/fb4/Move/privat/cur/* $HOME/data/aktuell/it/mail/new/
        ${unstable.isync}/bin/mbsync -a
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
