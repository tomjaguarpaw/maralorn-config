{ pkgs, config, lib, ... }:
let
  inherit (import ../lib) writeHaskellScript get-niv-path gcRetentionDays;
  inherit (import ../lib/update-home.nix) update-home;
  configPath = "/home/${config.home.username}/git/config";
in {
  home = {
    packages = builtins.attrValues {
      inherit get-niv-path;
      update-home = update-home configPath;
    };
  };
}
