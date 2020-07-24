{ pkgs, config, ... }:
let
  notesDir = "${config.home.homeDirectory}/git/zettelkasten";
  cmd = "${pkgs.myHaskellPackages.neuron}/bin/neuron -d ${notesDir} rib -wS";
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
