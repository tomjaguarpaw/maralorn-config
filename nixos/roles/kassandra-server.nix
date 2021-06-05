{ pkgs, ... }:
let
  serverPath = "/var/cache/gc-links/kassandra-server";
in
{
  systemd.services.kassandra = {
    wantedBy = [ "multi-user.target" ];
    description = "Kassandra Server";
    path = [ pkgs.coreutils pkgs.taskwarrior ];
    serviceConfig = {
      WorkingDirectory = serverPath;
      ExecStart = "${serverPath}/backend -b '::1' ";
      Restart = "always";
      User = "maralorn";
    };
  };
}
