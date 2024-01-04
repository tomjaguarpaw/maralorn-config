{ pkgs, config, ... }:
{
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      image = "~/.config/wallpaper";
      indicator-caps-lock = true;
      hide-keyboard-layout = true;
      indicator = true;
      clock = true;
      datestr = "${config.m-0.hostName}";
      scaling = "fill";
    };
  };
}
