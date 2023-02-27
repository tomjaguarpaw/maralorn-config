flake-inputs: {config, ...}: {
  imports = [flake-inputs.home-manager.nixosModules.home-manager];
  home-manager = {
    backupFileExtension = "home-manager-backup";
    useGlobalPkgs = true;
    users.maralorn = flake-inputs.self.homeConfigurations.${config.networking.hostName};
  };
}
