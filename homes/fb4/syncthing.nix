{ pkgs, ... }:
{
  systemd.user = {
    services.syncthing = {
      Unit = {
        Description = "Syncthing";
        ConditionHost = "fb04217";
        Wants= "syncthing-inotify.service";
      };

      Service = {
        ExecStart="${pkgs.syncthing}/bin/syncthing -no-browser -no-restart -logflags=0";
        Restart="on-failure";
        SuccessExitStatus="3 4";
        RestartForceExitStatus="3 4";
      };
    };
  };
}
