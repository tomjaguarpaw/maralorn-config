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
      grace = 10;
      clock = true;
      fade-in = 10;
      datestr = "${config.m-0.hostName}";
    };
  };
}
