{pkgs, ...}: {
  systemd.user = {
    services.fetch-banking = {
      Unit.Description = "Fetch banking";
      Service = {
        Type = "oneshot";
        ExecStart = toString (
          pkgs.writeShellScript "fetch-banking" ''
            cd ~/git/buchhaltung
            exec ${pkgs.nix}/bin/nix run ".#autoupdate"
          ''
        );
      };
    };
    timers.fetch-banking = {
      Unit.Description = "Fetch banking";
      Timer.OnCalendar = "23:00";
      Install.WantedBy = ["timers.target"];
    };
  };
}
