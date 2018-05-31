{ lib, pkgs, config, ... }:
with lib;
let
  sleep-nag = pkgs.writeShellScriptBin "sleep-nag" ''
while true
do
    if [[ `date +%H` -ge 23 ]] || [[ `date +%H` -lt 6 ]]; then
      ${pkgs.eventd}/bin/eventc notification kassandra -d "title='Es ist $(date +%H:%M) Uhr: Zeit ins Bett zu gehen!'" -d "message='Du kannst das hier auch morgen tun!'"
    fi
    sleep 10m
done
'';
in {

options.m-0.sleep-nag.enable = mkEnableOption "Sleep Nag";

config = mkIf config.m-0.sleep-nag.enable {
  systemd.user = {
    services.sleep-nag = {
      Unit = {
        Description = "Sleep nag";
      };
      Service = {
        ExecStart="/bin/sh ${sleep-nag}/bin/sleep-nag";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
};

}
