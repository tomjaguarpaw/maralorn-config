{ config, lib, ... }:
{
  systemd.services."test-and-bump-config" = {
    startAt = "20:30";
    serviceConfig = {
      Type = "oneshot";
      User = "maralorn";
      WorkingDirectory = "/var/cache/gc-links";
      ExecStart = "${(import ./test-lib.nix).test-and-bump-config}/bin/test-and-bump-config";
    };
  };
}
