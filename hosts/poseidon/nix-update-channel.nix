{ pkgs, ... }:
{
  systemd.user = {
    services.nix-update = {
      Unit = {
        Description = "Update nix-channel";
      };

      Service = {
        Type = "oneshot";
        ExecStart="${pkgs.nix}/bin/nix-channel --update";
      };
    };
    timers.nix-update = {
      Timer = {
        OnCalendar = "22:00";
        Persistent = "true";
      };
    };
  };
}
