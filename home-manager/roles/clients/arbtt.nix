{ pkgs, lib, ... }:
let
  package = pkgs.haskellPackages.arbtt;
in
{
  home.packages = [ package ];
  systemd.user = {
    timers.my-arbtt-capture = {
      Timer.OnCalendar = "minutely";
      Install.WantedBy = [ "timers.target" ];
    };
    services = {
      my-arbtt-capture = {
        Unit.Description = "Capture active window list for arbtt";
        Service = {
          ExecStart =
            pkgs.writers.writeHaskell "my-arbtt-capture"
              { libraries = builtins.attrValues pkgs.myHaskellScriptPackages; }
              (
                builtins.replaceStrings
                  [
                    "\"lswt\" -- NIX_BIN"
                    "\"jq\" -- NIX_BIN"
                    "\"arbtt-import\" -- NIX_BIN"
                  ]
                  [
                    "\"${lib.getExe pkgs.lswt}\""
                    "\"${lib.getExe pkgs.jq}\""
                    "\"${lib.getExe' package "arbtt-import"}\""
                  ]
                  (builtins.readFile ./my-arbtt-capture.hs)
              );
          Type = "oneshot";
        };
      };
    };
  };
}
