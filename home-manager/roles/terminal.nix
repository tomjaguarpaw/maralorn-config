{pkgs, ...}: let
  my-lib = import ../../lib;
  theme = my-lib.themes.default;
in {
  home.sessionVariables.TERMINAL = "${pkgs.foot}/bin/footclient";
  home.packages = [
    (pkgs.runCommandLocal "fake-gnome-terminal" {} ''
      mkdir -p $out/bin
      ln -s ${pkgs.foot}/bin/footclient $out/bin/gnome-terminal
    '')
  ];
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      csd = {
        preferred = "client";
        size = "0";
        border-width = "1";
        color = "af0000aa";
      };
      main = {
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
