{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
in {
  services.laminar.cfgFiles.jobs = {
    "test-config.run" = pkgs.writeShellScript "test-config" ''
      set -ex
      export PATH=${lib.makeBinPath path}:$PATH
      export NIX_PATH="/etc/nix-path:nixos-config=/etc/nixos/configuration.nix"
      cd /var/cache/gc-links
      ${pkgs.test-config}/bin/test-config
      ${pkgs.systemd}/bin/systemctl start --no-block update-config
    '';
  };
}
