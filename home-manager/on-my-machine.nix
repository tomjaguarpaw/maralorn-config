{ pkgs, config, lib,  ... }:
let
  inherit (import ../common/lib.nix) writeHaskellScript getNivPath;
  configPath = "/etc/nixos";
  gcRetentionDays = 5;
  update-home-manager = (import ./lib.nix).update-home-manager configPath;
  system-maintenance = writeHaskellScript
    { name = "system-maintenance"; bins = [ pkgs.nix pkgs.git update-home-manager ];} ''
    main = do
      git "-C" "${configPath}" "pull"
      exe "sudo" "update-system"
      update_home_manager
      exe "sudo" "nix-collect-garbage" "--delete-older-than" "${toString gcRetentionDays}d"
      nix "optimise-store"
  '';
in
{
  home.packages = builtins.attrValues {
    inherit system-maintenance update-home-manager getNivPath;
  };
}
