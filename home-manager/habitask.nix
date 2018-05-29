{ pkgs, ... }:
let
  habitask = with pkgs; with rustPlatform; buildRustPackage rec {
     name = "habitask";
     version = "0.1.0";
     src = ~/data/aktuell/it/code/habitask;
     depsSha256 = "0clac943ajxns64jkdcg312a4x4jgd239jb4yd5qm32nnkj62ym7";
     cargoSha256 = "0clac943ajxns64jkdcg312a4x4jgd239jb4yd5qm32nnkj62ym7";
     buildInputs = [ openssl pkgconfig ];
  };
in {
  systemd.user = {
    services.habitask = {
      Unit = {
        Description = "Update habitica Tasks";
      };
      Service = {
        Type = "oneshot";
        ExecStart="{habitask}/bin/habitask";
      };
    };
    timers.habitask = {
      Timer.OnCalendar = "6:00";
    };
  };
}
