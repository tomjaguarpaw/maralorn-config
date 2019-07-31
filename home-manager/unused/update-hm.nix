{ pkgs, ... }:
let
  path = "https://github.com/rycee/home-manager/archive/release-17.09.tar.gz";
  home-manager = (import ../../home-manager {
    inherit pkgs;
    inherit path;
  });
in {
  systemd.user = {
    services.update-hm = {
      Unit = { Description = "Update home-manager"; };

      Service = {
        Type = "oneshot";
        ExecStart = "${home-manager}/bin/home-manager switch";
      };
    };
    timers.update-hm = {
      Timer = {
        OnCalendar = "22:15";
        Persistent = "true";
      };
    };
  };
}
