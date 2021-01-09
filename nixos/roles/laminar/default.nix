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
  imports = [ ./kassandra.nix ./test-config.nix ];
  config = {
    services.laminar.cfgFiles = {
      scripts = {
        "nix-jobs" = pkgs.writeHaskell "nix-jobs" {
          libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
          ghcEnv.PATH = "${lib.makeBinPath [ pkgs.laminar pkgs.nix ]}:$PATH";
          ghcArgs = [ "-threaded" ];
        } (builtins.readFile ./nix-jobs.hs);
      };
      jobs = {
        "nix-build.run" = pkgs.writeShellScript "nix-build" ''
          set -e
          PATH=${
            lib.makeBinPath [ pkgs.laminar pkgs.nix ]
          }:$PATH nix-jobs realise-here "$DERIVATION"
        '';
      };
      after = pkgs.writeShellScript "after-all-jobs-script" ''
        if [[ "$JOB$RESULT" != "nix-buildsuccess" ]]; then
        LAMINAR_URL="https://ci.maralorn.de"
        exec 100>${stateDir}/matrix-lock
        ${pkgs.utillinux}/bin/flock -w 10 100
        trap 'rm -f ${stateDir}/matrix-lock' EXIT
        ${pkgs.matrix-commander}/bin/matrix-commander -c ${stateDir}/matrix-credentials.json -s ${stateDir}/matrix-secrets-store <<EOF
        $JOB #$RUN: $RESULT https://ci.m-0.eu/jobs/$JOB/$RUN
        $(if [[ $RESULT == "failed" ]]; then echo -e 'maralorn'; ${pkgs.curl}/bin/curl -m5 -s $LAMINAR_URL/log/$JOB/$RUN | tail; fi)
        EOF
        fi
        true
      '';
      contexts = {
        "default.conf" = builtins.toFile "default.conf" "EXECUTORS=16";
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
        LimitNOFILE = "10240";
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
          "ci.m-0.eu" = {
            forceSSL = true;
            enableACME = true;
            extraConfig = "return 301 https://ci.maralorn.de$request_uri;";
          };
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
