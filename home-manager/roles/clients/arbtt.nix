{ pkgs, lib, ... }:
let
  package = pkgs.haskellPackages.arbtt;
in
{
  home.packages = [ package ];
  home.file.".arbtt/categorize.cfg".text = ''
    $idle > 60 ==> tag inactive,
    tag Program:$current.program,
    current window $title =~ m!/git/([^/]*)! ==> tag Project:$1,
  '';
  systemd.user = {
    timers.my-arbtt-capture = {
      Timer.OnCalendar = "*:*:10/20";
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
