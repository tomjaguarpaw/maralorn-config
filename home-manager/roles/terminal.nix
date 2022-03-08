{pkgs, ...}: let
  my-lib = import ../../lib;
  theme = my-lib.themes.default;
in {
  home.sessionVariables.TERMINAL = "${pkgs.foot}/bin/foot";
  home.packages = [
    (pkgs.runCommandLocal "fake-gnome-terminal" {} ''
      mkdir -p $out/bin
      ln -s ${pkgs.foot}/bin/foot $out/bin/gnome-terminal
    '')
  ];
  programs.foot = {
    enable = true;
    settings = {
      csd = {
        preferred = "none";
      };
      main = {
        term = "xterm-256color";
        font = "monospace:size=10.5";
        dpi-aware = "no";
        include = "${pkgs.foot.themes}/share/foot/themes/selenized-white";
      };
      mouse = {
        hide-when-typing = "yes";
      };
      scrollback = {
        lines = 100000;
      };
    };
  };
}
