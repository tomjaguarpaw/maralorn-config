{ pkgs, ... }:
{
  systemd.user = {
    services.nix-gc = {
      Unit = {
        Description = "Collect garbage";
      };

      Service = {
        Type = "oneshot";
        ExecStart="${pkgs.nix}/bin/nix-collect-garbage --delete-older-than 5d";
      };
    };
    timers.nix-gc = {
      Timer = {
        OnCalendar = "22:00";
        Persistent = "true";
      };
    };
  };
}
