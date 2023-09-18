{ pkgs, config, ... }:
{
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      image = "~/media/images/lockscreen.jpg";
      indicator-caps-lock = true;
      hide-keyboard-layout = true;
      indicator = true;
      clock = true;
      datestr = "${config.m-0.hostName}";
    };
  };
}
