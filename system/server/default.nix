{ config, pkgs, lib, ... }:
let user = "maralorn";
in {

  imports = [ ./init_ssh.nix ];

  systemd.services.test-and-update = {
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
      ${(import ../../lib/test.nix).test-config}/bin/test-config
      /run/wrappers/bin/sudo -u ${user} git -C /etc/nixos pull
      ${
        (import ../../lib/update-system.nix
        config.system.build.nixos-rebuild).update-system
      }/bin/update-system
      /run/wrappers/bin/sudo -u ${user} update-home
    '';
  };
  nix = {
    gc.automatic = true;
    optimise.automatic = true;
  };

}
