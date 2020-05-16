{ pkgs, ... }: {
  systemd.user.services.kassandra = {
    Unit = { Description = "Kassandra Server"; };
    Service = {
      WorkingDirectory = "/var/www/kassandra";
      ExecStart = "/var/www/kassandra/backend -b '::1' ";
      Restart = "always";
      Environment = "PATH=${pkgs.coreutils}/bin/:${pkgs.taskwarrior}/bin";
    };
    Install = { WantedBy = [ "default.target" ]; };
  };
}
