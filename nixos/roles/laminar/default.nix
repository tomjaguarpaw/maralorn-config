{ pkgs, lib, config, ... }:
let
  inherit (lib) types mkOption;
  stateDir = "/var/lib/laminar";
  cfgDir = "${stateDir}/cfg";
  cfg = config.services.laminar;
in
{
  options = {
    services.laminar = {
      cfgFiles = mkOption {
        type =
          let valueType = with types; oneOf [ path (attrsOf valueType) ];
          in valueType;
        default = { };
        description = ''
          Every entry will be copied to /var/lib/laminar/cfg/<name>

          Can be used to define jobs, helper scripts, etc.
        '';
      };
    };
  };
  imports = [ ./kassandra.nix ./test-config.nix ./projects.nix ];
  config = {
    services.laminar.cfgFiles = {
      env = builtins.toFile "laminar-env" ''
        TIMEOUT=14400
      '';
      after = pkgs.writeShellScript "after-all-jobs-script" ''
        LAMINAR_URL="https://ci.maralorn.de"
        exec 100>${stateDir}/matrix-lock
        ${pkgs.utillinux}/bin/flock -w 10 100
        trap 'rm -f ${stateDir}/matrix-lock' EXIT
        ${pkgs.matrix-commander}/bin/matrix-commander -c ${stateDir}/matrix-credentials.json -s ${stateDir}/matrix-secrets-store <<EOF
        $JOB #$RUN: $BRANCH$DERIVATION $RESULT https://ci.m-0.eu/jobs/$JOB/$RUN
        $(if [[ $RESULT == "failed" ]]; then echo -e 'maralorn'; ${pkgs.curl}/bin/curl -m5 -s $LAMINAR_URL/log/$JOB/$RUN | tail; fi)
        EOF
        echo "Result was: $RESULT"
      '';
      contexts = {
        "default.conf" = builtins.toFile "default.conf" "EXECUTORS=8";
      };
    };
    users = {
      groups.laminar = { };
      users.laminar = {
        group = "laminar";
        home = stateDir;
        isSystemUser = true;
      };
    };
    environment.systemPackages = [ pkgs.laminar ];
    systemd.services.laminar = {
      wantedBy = [ "multi-user.target" ];
      description = "Laminar continuous integration service";
      serviceConfig = {
        WorkingDirectory = stateDir;
        ExecStart = "${pkgs.laminar}/bin/laminard";
        User = "laminar";
        StateDirectory = "laminar";
        LimitNOFILE = "1024000";
      };
      after = [ "network.target" ];
      preStart = "ln -sfT ${pkgs.setToDirectories cfg.cfgFiles} ${cfgDir}";
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
