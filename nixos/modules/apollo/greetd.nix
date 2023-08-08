{ pkgs, lib, ... }:
{
  services.greetd.settings.default_session.command = "${
      lib.getExe pkgs.greetd.tuigreet
    } --time --cmd launch-hyprland --remember";
}
