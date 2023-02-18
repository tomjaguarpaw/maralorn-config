{
  pkgs,
  config,
  ...
}: {
  home.sessionVariables.TERMINAL = "${pkgs.foot}/bin/foot";
  home.packages = [
    (pkgs.recursiveLinkFarm "fake-gnome-terminal" {
      "bin/gnome-terminal" = config.home.sessionVariables.TERMINAL;
    })
  ];
  programs.foot = {
    settings = {
      main = {
        font = "monospace:pixelsize=12";
        include = "${pkgs.foot.themes}/share/foot/themes/catppuccin";
      };
      csd = {
        preferred = "client";
        size = "0";
        border-width = "1";
        color = "af0000aa";
      };
      mouse = {
        hide-when-typing = "yes";
      };
    };
    enable = true;
  };
}
