{ pkgs, lib, ... }:
let
  conf = {
    homeserverUrl = "https://matrix.maralorn.de";
    userID = "@marabot:maralorn.de";
    accessToken = pkgs.privateValue "" "matrix/marabot-token";
    roomID = "!MOUlKTKyWKUMyHkUEd:maralorn.de";
  };
in

{

  systemd.user = {
    timers.sleep-nag = {
      Timer.OnCalendar = "22:30";
      Install.WantedBy = [ "timers.target" ];
    };

    services.sleep-nag = {
      Unit.Description = "Sleep Nag";
      Service = {
        ExecStart = pkgs.writeShellScript "sleep-nag" "echo Guten Abend. Es ist 22:30 Uhr. | ${lib.getExe pkgs.neosay} -config ${builtins.toFile "config.json" (builtins.toJSON conf)}";
        Type = "oneshot";
      };
    };

  };
}
