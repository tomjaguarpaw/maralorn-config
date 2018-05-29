{ pkgs, ... }:
let
morgenreport-script = pkgs.writeShellScriptBin "morgenreport" ''
    cd $HOME/data/aktuell/media/ebooks/morgenreport/
    DATE=`date +%Y-%m-%d`
    PATH=$PATH:/run/wrappers/bin/
    PATH=$PATH:${pkgs.imagemagickBig}/bin
    PATH=$PATH:${pkgs.qt5.qtbase}/bin
    PATH=$PATH:${pkgs.qt5.qtsvg}/bin
    ${pkgs.calibre}/bin/ebook-convert $HOME/data/aktuell/it/code/calibre-recipes/morgenreport.recipe morgenreport-$DATE.mobi --output-profile=kindle_pw3
    echo "File created, sending to kindle now …"
    echo 'Siehe Anhang' | ${pkgs.mutt}/bin/mutt -s "Morgenreport $DATE" -a morgenreport-$DATE.mobi -- maralorn@kindle.com
  '';
in {
  home.packages = [ morgenreport-script];
  systemd.user = {
    services.morgenreport = {
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
