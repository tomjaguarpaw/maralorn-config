{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      river
      guile_2_2
      ristate
      riverguile
      wlopm
      wlr-randr
      swaybg
      kanshi
      jaq # Für eww
      river-tag-overlay
      lswt
      light
      ;
  };
  xdg.configFile = {
    "river/init".source = ./river-init.sh;
    "river/layout.scm".source = ./layout.scm;
  };
  systemd.user.targets.river-session.Unit.BindsTo = "graphical-session.target";
}
