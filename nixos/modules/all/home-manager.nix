{
  config,
  pkgs,
  mylib,
  ...
}:
{
  home-manager = {
    backupFileExtension = "home-manager-backup";
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit mylib;
    };
    users.maralorn = {
      imports = [
        pkgs.flake-inputs.nix-index-database.hmModules.nix-index
        (import ./../../../home-manager/machines.nix mylib)
        .${config.networking.hostName}
      ];
    };
  };
}
