{ pkgs, ... }:
let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in {
  systemd.user = {
    services.morgenreport =
    let
    morgenreport-script = pkgs.writeShellScriptBin "morgenreport" ''
        cd $HOME/data/aktuell/media/ebooks/morgenreport/
        DATE=`date +%Y-%m-%d`
        ${pkgs.calibre}/bin/ebook-convert $HOME/data/aktuell/it/code/calibre-recipes/morgenreport.recipe morgenreport-$DATE.mobi --output-profile=kindle_pw3
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
  };
}
