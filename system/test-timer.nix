{ pkgs, config, lib, ... }:
let
  user = "maralorn";
  inherit (import ../lib/update-system.nix {
    nixos-rebuild = config.system.build.nixos-rebuild;
    inherit pkgs;
  })
    update-system;
in {

  systemd.services = {
    test-and-update = {
      environment.NIX_PATH =
        "/etc/nix-path:nixos-config=/etc/nixos/configuration.nix";
      path = [ pkgs.nix pkgs.gnutar pkgs.gzip pkgs.git pkgs.git-crypt ];
      restartIfChanged = false;
      unitConfig.X-StopOnRemoval = false;
      serviceConfig = {
        Type = "oneshot";
        WorkingDirectory = "/var/cache/gc-links";
        Restart = "on-failure";
        RestartSec = 1;
      };
      unitConfig = {
        StartLimitIntervalSec=180;
        StartLimitBurst=3;
      };
      script = ''
        ${pkgs.test-config}/bin/test-config
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull
        result-system-hera/bin/switch-to-configuration switch
        /run/wrappers/bin/sudo -u ${user} result-home-manager-hera/default/activate
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
