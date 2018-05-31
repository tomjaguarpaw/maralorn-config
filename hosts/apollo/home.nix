{ pkgs, ... }:
{

imports = [
  ../../home-manager
];

m-0.laptop.enable = true;
m-0.sleep-nag.enable = true;
m-0.battery.enable = true;
m-0.latex.enable = true;
m-0.accounting.enable = true;
m-0.graphical.enable = true;
m-0.rustdev.enable = true;
m-0.taskwarrior.enable = true;
m-0.eventd.enable = true;

}
