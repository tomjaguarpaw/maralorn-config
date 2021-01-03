{ pkgs, lib, config, ... }:
let
  types = lib.types;
  stateDir = "/var/lib/laminar";
  jobsDir = "${stateDir}/cfg/jobs";
  cfg = config.services.laminar;
in {
  options = {
    services.laminar.jobs = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      description = ''
        CI jobs statically known to laminar.
        Every attribute will be copied to /var/lib/laminar/cfg/jobs/<name>
      '';
    };
  };
    imports = [./kassandra.nix ];
  config = {
    users = {
      groups.laminar = { };
      users.laminar = {
        group = "laminar";
        home = stateDir;
      };
    };
    environment.systemPackages = [ pkgs.laminar ];
    systemd.services.laminar = {
      enable = true;
      description = "Laminar continuous integration service";
      serviceConfig = {
        WorkingDirectory = stateDir;
        ExecStart = "${pkgs.laminar}/bin/laminard";
        DynamicUser = false;
        User = "laminar";
        StateDirectory = "laminar";
      };
      after = [ "network.target" ];
      preStart = ''
        mkdir -p ${jobsDir}
        ${lib.concatStrings (lib.mapAttrsToList (key: value: ''
          ln -sf ${value} ${jobsDir}/${key}
        '') cfg.jobs)}'';
    };
    services = {
      nginx = {
        virtualHosts = {
          "ci.maralorn.de" = {
            forceSSL = true;
            enableACME = true;
            locations."/".proxyPass = "http://[::1]:8080";
          };
        };
      };
    };
  };
}
