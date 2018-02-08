{ pkgs, ... }:
let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
  habitask = with pkgs; with rustPlatform; buildRustPackage rec {
     name = "habitask";
     version = "0.1.0";
     src = ~/data/aktuell/it/code/habitask;
     depsSha256 = "0clac943ajxns64jkdcg312a4x4jgd239jb4yd5qm32nnkj62ym7";
     cargoSha256 = "0clac943ajxns64jkdcg312a4x4jgd239jb4yd5qm32nnkj62ym7";
     buildInputs = [ openssl pkgconfig ];
  };
in {
  imports = [
    ../snippets/everywhere.nix
    ../snippets/my-systems.nix
  ];
  home.packages = [ habitask ];

  systemd.user = {
    services.morgenreport =
    let
    morgenreport-script = pkgs.writeShellScriptBin "morgenreport" ''
        cd $HOME/data/aktuell/media/ebooks/morgenreport/
        DATE=`date +%Y-%m-%d`
        ${unstable.calibre}/bin/ebook-convert $HOME/data/aktuell/it/code/calibre-recipes/morgenreport.recipe morgenreport-$DATE.mobi --output-profile=kindle_pw3
        echo "File created, sending to kindle now …"
        PATH=$PATH:/run/wrappers/bin/
        echo 'Siehe Anhang' | ${pkgs.mutt}/bin/mutt -s "Morgenreport $DATE" -a morgenreport-$DATE.mobi -- maralorn@kindle.com
      '';
    in {
      Unit = {
        Description = "Send morgenreport to kindle";
      };

      Service = {
        Type = "oneshot";
        ExecStart="/bin/sh ${morgenreport-script}/bin/morgenreport";
      };
    };
    timers.morgenreport = {
      Timer = {
        OnCalendar = "20:00";
      };
    };

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
