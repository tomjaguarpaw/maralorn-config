{ pkgs, config, lib, ... }:
let
  user = "maralorn";
  inherit (import ../../lib/update-system.nix {
    nixos-rebuild = config.system.build.nixos-rebuild;
    inherit pkgs;
  })
    update-system;
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
        StartLimitIntervalSec=180;
        StartLimitBurst=3;
      };
      script = ''
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull
        /var/cache/gc-links/result-system-hera/bin/switch-to-configuration switch
        /run/wrappers/bin/sudo -u ${user} /var/cache/gc-links/result-home-manager-hera/default/activate
      '';
    };
    test-config = {
      environment.NIX_PATH =
        "/etc/nix-path:nixos-config=/etc/nixos/configuration.nix";
      path = [ pkgs.nix pkgs.gnutar pkgs.gzip pkgs.git pkgs.git-crypt ];
      serviceConfig = {
        Type = "oneshot";
        WorkingDirectory = "/var/cache/gc-links";
      };
      script = ''
        ${pkgs.test-config}/bin/test-config
        ${pkgs.systemd}/bin/systemctl start --no-block update-config
      '';
    };

    test-and-bump-config = {
      startAt = "03:45";
      path = [ pkgs.nix pkgs.gnutar pkgs.gzip pkgs.git pkgs.git-crypt ];
      serviceConfig = {
        Type = "oneshot";
        WorkingDirectory = "/var/cache/gc-links";
        ExecStart =
          "${pkgs.test-config}/bin/test-config bump";
      };
    };
  };
}
