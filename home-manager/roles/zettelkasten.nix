{ pkgs, config, ... }:
let
  notesDir = "${config.home.homeDirectory}/git/zettelkasten";
  cmd = "${pkgs.myHaskellPackages.neuron}/bin/neuron -d ${notesDir} rib -w -s 127.0.0.1:8002";
in {
  systemd.user.services.neuron = {
    Unit.Description = "Neuron zettelkasten service";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = cmd;
      Restart = "always";
    };
  };
}
