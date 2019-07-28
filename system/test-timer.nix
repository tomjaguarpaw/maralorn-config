{ pkgs, config, lib, ... }:
{
  systemd.services."test-and-bump-config" = {
    startAt = "20:30";
    path = [ pkgs.nix pkgs.gnutar pkgs.gzip pkgs.git ];
    serviceConfig = {
      Type = "oneshot";
      WorkingDirectory = "/var/cache/gc-links";
      ExecStart = "${(import ./test-lib.nix).test-and-bump-config}/bin/test-and-bump-config";
    };
  };
}
