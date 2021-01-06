{ pkgs, config, lib, ... }:
let user = "maralorn";
in {
  systemd.services = {
    update-config = {
      path = [ pkgs.git pkgs.nix pkgs.git-crypt ];
      restartIfChanged = false;
      unitConfig.X-StopOnRemoval = false;
      serviceConfig = {
        Type = "oneshot";
        Restart = "on-failure";
        RestartSec = 1;
      };
      unitConfig = {
        StartLimitIntervalSec = 180;
        StartLimitBurst = 3;
      };
      script = ''
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull --ff-only
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos submodule update --init
        /var/cache/gc-links/result-system-hera/bin/switch-to-configuration switch
        /run/wrappers/bin/sudo -u ${user} /var/cache/gc-links/result-home-manager-hera/default/activate
      '';
    };

    bump-and-test-config = {
      startAt = "03:45";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.laminar}/bin/laminarc queue bump-and-test-config";
      };
    };
  };
}
