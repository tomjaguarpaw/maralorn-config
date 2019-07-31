{ config, lib, pkgs , ... }:
let
  inherit (import ../common/pkgs.nix) eventd;
in
{
  systemd.user = {
    services.update_tasks = {
      Unit = {
        Description = "Update taskwarrior tasks";
      };
      Service = {
        Type = "oneshot";
        Environment="PATH=${pkgs.taskwarrior}/bin:${eventd}/bin";
        ExecStart= "${config.home.homeDirectory}/.cargo/bin/update_tasks";
      };
    };
    timers.update_tasks = {
      Timer = {
        OnCalendar = "hourly";
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };

}
