{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "launch-wm" ''
      exec river &>> /run/user/$UID/river.log
    '')
  ];
  services.greetd.enable = true;
  programs.sway.enable = true; # For swaylock pam files …
}
