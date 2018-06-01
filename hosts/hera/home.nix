{ pkgs, ... }:
{

imports = [
  ../../home-manager
];

m-0.rustdev.enable = true;
m-0.taskwarrior.enable = true;
m-0.eventd.enable = true;

}
