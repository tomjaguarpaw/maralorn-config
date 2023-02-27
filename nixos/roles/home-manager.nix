flake-inputs: {config, ...}: {
  imports = [flake-inputs.home-manager.nixosModules.home-manager];
  home-manager = {
    backupFileExtension = "home-manager-backup";
    useGlobalPkgs = true;
    users.maralorn = (import ../../home-manager/machines.nix).${config.networking.hostName}.default;
  };
}
