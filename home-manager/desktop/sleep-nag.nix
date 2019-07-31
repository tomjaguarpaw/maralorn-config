{ lib, pkgs, config, ... }:
let
  inherit (import ../../common/pkgs.nix) eventd;
  sleep-nag = pkgs.writeScript "sleep-nag" ''
    #!${pkgs.stdenv.shell}

    while true
    do
        if [[ `date +%H` -ge 23 ]] || [[ `date +%H` -lt 6 ]]; then
          ${eventd}/bin/eventc notification kassandra -d "title='Es ist $(date +%H:%M) Uhr: Zeit ins Bett zu gehen!'" -d "message='Du kannst das hier auch morgen tun!'"
        fi
        sleep 10m
    done
  '';
in {

  systemd.user = {
    services.sleep-nag = {
      Unit = { Description = "Sleep nag"; };
      Service = { ExecStart = toString sleep-nag; };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };

}
