{ pkgs, ... }: {
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
            shake fetch
            if [[ "$(git status --porcelain */raw)" != "" ]]; then
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
      Timer.OnCalendar = "hourly";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
