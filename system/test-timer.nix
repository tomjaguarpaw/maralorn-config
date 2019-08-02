{ pkgs, config, lib, ... }:
let
  user = "maralorn";
  inherit (import ../lib/test.nix) test-config;
  inherit (import ../lib/update-system.nix config.system.build.nixos-rebuild)
    update-system;
  update-home = (import ../lib/update-home.nix).update-home "/etc/nixos";
in {

  systemd.services = {
    test-and-update = {
      startAt = "2:45";
      environment.NIX_PATH =
        "/etc/nix-path:nixos-config=/etc/nixos/configuration.nix";
      path = [ pkgs.nix pkgs.gnutar pkgs.gzip pkgs.git pkgs.git-crypt ];
      restartIfChanged = false;
      unitConfig.X-StopOnRemoval = false;
      serviceConfig = {
        Type = "oneshot";
        WorkingDirectory = "/var/cache/gc-links";
      };
      script = ''
        ${test-config}/bin/test-config
        /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull
        ${update-system}/bin/update-system
        /run/wrappers/bin/sudo -u ${user} ${update-home}/bin/update-home
      '';
    };

    test-and-bump-config = {
      startAt = "20:30";
      path = [ pkgs.nix pkgs.gnutar pkgs.gzip pkgs.git pkgs.git-crypt ];
      serviceConfig = {
        Type = "oneshot";
        WorkingDirectory = "/var/cache/gc-links";
        ExecStart =
          "${(import ../lib/test.nix).test-config}/bin/test-config bump";
      };
    };
  };
}
