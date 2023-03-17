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
        font = "Symbols Nerd Font Mono:pixelsize=12,CozetteVector:pixelsize=12";
        include = toString (pkgs.writeText "foot-theme" ''
          ${builtins.readFile "${pkgs.foot.themes}/share/foot/themes/catppuccin"}
          background=000000
          alpha=0.9
        '');
      };
      csd = {
        preferred = "client";
        size = "0";
        border-width = "1";
        color = "ff${config.m-0.colors.accent}";
      };
      mouse = {
        hide-when-typing = "yes";
      };
    };
    enable = true;
  };
}
