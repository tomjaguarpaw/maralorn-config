{
  config,
  pkgs,
  prelude,
  ...
}:
{
  home-manager = {
    backupFileExtension = "home-manager-backup";
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit prelude;
    };
    users.maralorn = {
      imports = [
        pkgs.flake-inputs.nix-index-database.hmModules.nix-index
        (import ./../../../home-manager/machines.nix prelude)
        .${config.networking.hostName}
      ];
    };
  };
}
