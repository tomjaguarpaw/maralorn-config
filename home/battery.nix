{ lib, pkgs, config, ... }:
let
  battery-watch = pkgs.writeScript "battery-watch" ''
    #!${pkgs.stdenv.shell}

    critical_level=20    #percent

    export PATH=$PATH:${pkgs.coreutils}/bin:${pkgs.gnugrep}/bin:${pkgs.gnused}/bin

    while true
    do
        if [ "$(${pkgs.acpi}/bin/acpi -a | grep -o off)" == "off" ]; then
            battery_level=`${pkgs.acpi}/bin/acpi -b | sed 's/.*[dg], //g;s/\%,.*//g'`
            if [ $battery_level -le $critical_level ]; then
              ${pkgs.libnotify}/bin/notify-send 'Battery level is low!' "Only $battery_level% of the charge remains."
            else
              ${pkgs.libnotify}/bin/notify-send 'Battery level is discharging!' "Only $battery_level% of the charge remains."
              sleep 18m
            fi
        fi
        sleep 2m
    done
  '';
in {

  systemd.user = {
    services.battery = {
      Unit = { Description = "Watch battery state and warn user"; };
      Service = { ExecStart = toString battery-watch; };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };

}
