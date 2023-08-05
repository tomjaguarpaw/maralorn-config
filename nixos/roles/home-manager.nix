flake-inputs:
{ config, ... }:
{
  home-manager = {
    backupFileExtension = "home-manager-backup";
    useGlobalPkgs = true;
    useUserPackages = true;
    users.maralorn =
      (import ../../home-manager/machines.nix flake-inputs)
      .${config.networking.hostName}.default;
  };
}
