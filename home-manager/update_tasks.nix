{ config, lib, pkgs, ... }: {
  systemd.user = {
    services.update_tasks = {
      Unit = { Description = "Update taskwarrior tasks"; };
      Service = {
        Type = "oneshot";
        Environment = "PATH=${pkgs.taskwarrior}/bin:${pkgs.git}/bin";
        ExecStart = "${config.home.homeDirectory}/.cargo/bin/update_tasks";
      };
    };
    timers.update_tasks = {
      Timer = { OnCalendar = "hourly"; };
      Install = { WantedBy = [ "timers.target" ]; };
    };
  };

}
