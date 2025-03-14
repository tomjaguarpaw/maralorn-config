{
  pkgs,
  lib,
  config,
  ...
}:
{

  options.status-script.env = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [ ];
    description = lib.mdDoc "Packages in the status-script environment.";
  };

  config.systemd.user.services.status-script = {
    Unit.Description = "status-script";
    Service = {
      Environment = "PATH=${lib.makeBinPath config.status-script.env}";
      ExecStart = lib.getExe' pkgs.status-script "status-script";
    };
  };
}
