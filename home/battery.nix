{ lib, pkgs, config, ... }:
let
  inherit (import ../pkgs) eventd;
  battery-watch = pkgs.writeScript "battery-watch" ''
    #!${pkgs.stdenv.shell}

    critical_level=20    #percent

    export PATH=$PATH:${pkgs.coreutils}/bin:${pkgs.gnugrep}/bin

    while true
    do
        if [ "$(${pkgs.acpi}/bin/acpi -a | grep -o off)" == "off" ]; then
            battery_level=`${pkgs.acpi}/bin/acpi -b | sed 's/.*[dg], //g;s/\%,.*//g'`
            if [ $battery_level -le $critical_level ]; then
              ${eventd}/bin/eventc critical battery -d "title='Battery level is low!'" -d "message='Only $battery_level% of the charge remains.'"
            else
              ${eventd}/bin/eventc notification battery -d "title='Battery is discharging!'" -d "message='Only $battery_level% of the charge remains.'"
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
