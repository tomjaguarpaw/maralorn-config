{pkgs, ...}: {
  systemd.user = {
    services.fetch-banking = {
      Unit.Description = "Fetch banking";
      Service = {
        Type = "oneshot";
        ExecStart = toString (
          pkgs.writeShellScript "fetch-banking" ''
            set -e
            cd ~/git/buchhaltung
            git pull
            ${pkgs.nix}/bin/nix develop -c shake fetch
            if [[ "$(git status --porcelain */raw */prices)" != "" ]]; then
               git add */raw
               git add */prices
               git commit -m "Load transactions and balances"
               git push
            fi
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
