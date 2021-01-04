{ pkgs, lib, config, ... }:
let
  inherit (lib) types mkOption;
  stateDir = "/var/lib/laminar";
  cfgDir = "${stateDir}/cfg";
  cfg = config.services.laminar;
in {
  options = {
    services.laminar = {
      cfgFiles = mkOption {
        type = let valueType = with types; oneOf [ path (attrsOf valueType) ];
        in valueType;
        default = { };
        description = ''
          Every entry will be copied to /var/lib/laminar/cfg/<name>

          Can be used to define jobs, helper scripts, etc.
        '';
      };
    };
  };
  imports = [ ./kassandra.nix ];
  config = {
    services.laminar.cfgFiles = {
      scripts = {
        "nix-jobs" = pkgs.writeHaskell "nix-jobs" {
          libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
          ghcEnv = {
            PATH = "${lib.makeBinPath [ pkgs.laminar pkgs.nix ]}:$PATH";
          };
        } (builtins.readFile ./nix-jobs.hs);
      };
      jobs = {
        "nix-build.run" = pkgs.writeShellScript "nix-build" ''
          set -e
          nix-jobs realize-here "$DERIVATION"
        '';
      };
    };
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
      preStart = let
        linkToPath = path: fileOrDir:
          (if types.path.check fileOrDir then
            [ "ln -sT ${fileOrDir} ${path}" ]
          else
            [ "mkdir -p ${path}" ] ++ lib.concatLists (lib.mapAttrsToList
              (dirName: content: linkToPath "${path}/${dirName}" content)
              fileOrDir));
        cfgDirContent = pkgs.runCommand "laminar-cfg-dir" { }
          (lib.concatStringsSep "\n" (linkToPath "$out" cfg.cfgFiles));
      in "ln -sfT ${cfgDirContent} ${cfgDir}";
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
