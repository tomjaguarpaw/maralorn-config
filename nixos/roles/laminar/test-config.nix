{ pkgs, lib, config, ... }:
let
  path = [ pkgs.git pkgs.nix pkgs.gnutar pkgs.gzip pkgs.openssh pkgs.laminar ];
  common = ''
    set -ex
    export PATH=${lib.makeBinPath path}:$PATH
    export NIX_PATH="/etc/nix-path:nixos-config=/etc/nixos/configuration.nix"
    export GIT_SSH_COMMAND="ssh -vv"
    cd /var/cache/gc-links
  '';
in {
  services.laminar.cfgFiles.jobs = {
    "test-config.run" = pkgs.writeShellScript "test-config" ''
      ${common}
      ${pkgs.test-config}/bin/test-config
      ${pkgs.systemd}/bin/systemctl start --no-block update-config
    '';
    "bump-and-test-config.run" = pkgs.writeShellScript "bump-and-test-config" ''
      ${common}
      ${pkgs.test-config}/bin/test-config bump
    '';
  };
}
