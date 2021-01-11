{ pkgs, ... }: {
  systemd.services.kassandra = {
    enable = true;
    description = "Kassandra Server";
    serviceConfig = let serverPath = "/var/cache/gc-links/kassandra-server";
    in {
      WorkingDirectory = serverPath;
      ExecStart = "${serverPath}/backend -b '::1' ";
      Restart = "always";
      Environment = "PATH=${pkgs.coreutils}/bin/:${pkgs.taskwarrior}/bin";
      User = "maralorn";
    };
  };
}
