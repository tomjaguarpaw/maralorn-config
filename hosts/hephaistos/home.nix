{ pkgs, config, ... }:
{
  imports = [
    ../../home-manager
  ];

  systemd.user.systemctlPath = "/usr/bin/systemctl";

programs.home-manager.enable = true;

m-0 = {
  hostName = "fb04217";
  #sleep-nag.enable = true;
  #latex.enable = true;
  #graphical.enable = true;
  #rustdev.enable = true;
  #taskwarrior = {
  #  enable = true;
  #  git_active = true;
  #};
  #update_tasks.enable = true;
  #eventd.enable = true;
  #pythia.enable = true;
};
home.packages = [
  (pkgs.writeShellScriptBin "maintenance" ''
    nix-channel --update
    home-manager switch
    nix-collect-garbage --delete-older-than 5
    nix-optimise
  '')
];

}
