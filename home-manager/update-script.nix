{ pkgs, config, lib,  ... }:
let
  inherit (import ../common/lib.nix) writeHaskellScript get-niv-path gcRetentionDays;
  inherit (import ./lib.nix) update-home;
  configPath = "/home/${config.home.username}/git/nixos/config";
  home-maintenance = writeHaskellScript
    { name = "home-maintenance"; imports = [ ]; bins = [ (update-home configPath) pkgs.nix pkgs.git];} ''
    main = do
      git "-C" "${configPath}" "pull"
      update_home
      nix_collect_garbage "--delete-older-than" "${toString gcRetentionDays}d"
      nix "optimise-store"
  '';
in
{
  home = {
    packages = builtins.attrValues {
      inherit home-maintenance get-niv-path;
      update-home = update-home configPath;
    };
  };
}
