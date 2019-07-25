{ pkgs, config, lib,  ... }:
let
  inherit (import ../common/lib.nix) writeHaskellScript;
  configPath = "/home/${config.home.username}/git/nixos/config";
  gcRetentionDays = 5;
  update-home-manager = (import ./lib.nix).update-home-manager configPath;
  user-maintenance = writeHaskellScript
    { name = "user-maintenance"; imports = [ ]; bins = [ update-home-manager pkgs.nix pkgs.git];} ''
    main = do
      git "-C" "${configPath}" "pull"
      update_home_manager
      nix_collect_garbage "--delete-older-than" "${toString gcRetentionDays}d"
      nix "optimise-store"
  '';
{
  home =
    sessionVariables = {
      NIX_PATH = "$HOME/.nix-path";
    };
    file = {
      home-manager-source = {
        target = ".nix-path/home-manager";
        source = sources.home-manager;
      };
      nixpkgsr-source = {
        target = ".nix-path/nixpkgs";
        source = sources.nixpkgs;
      };
      nixos = {
        target = ".nix-path/nixos";
        source = sources.nixpkgs;
      };
      unstable = {
        target = ".nix-path/unstable";
        source = sources.unstable;
      };
    };
    packages = builtins.attrValues {
      inherit user-maintenance update-home-manager;
    };
  };
}
