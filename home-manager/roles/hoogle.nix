{pkgs, ...}: {
  systemd.user.services.hoogle = {
    Unit.Description = "Hoogle server";
    Install.WantedBy = ["graphical-session.target"];
    Service = {
      ExecStart = "${pkgs.ghcWithPackages}/bin/hoogle server --local --links";
      Restart = "always";
    };
  };
}
