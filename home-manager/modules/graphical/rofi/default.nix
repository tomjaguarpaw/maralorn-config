{ pkgs, lib, config, ... }:
let
  workspaces = config.m-0.workspaces;
  terminal = config.m-0.terminal;
  colors = config.m-0.colors;
  rofiTask = pkgs.writeScriptBin "tasklauncher" (builtins.readFile ./tasklauncher.py);
in {
  home = {
    packages = with pkgs; [
      rofi
      rofiTask
      rofi-pass
    ];
  };
  programs = {
    rofi = {
      enable = true;
      extraConfig = ''
        rofi.modi: combi,window,drun,run,ssh,keys
        rofi.combi-modi: window,drun,run
        '';
      borderWidth = 0;
      separator = "none";
      fullscreen = false;
      terminal = terminal;
      yoffset = 19;
      location = "top";
      scrollbar = false;
      padding = 10;
      cycle = false;
      lines = 30;
      colors = {
        window = {
         background = "argb:c0${builtins.substring 1 6 colors.background}";
         border = colors.blue;
         separator = colors.blue;
        };
        rows = {
         normal = {
           background = colors.background;
           foreground = colors.foreground;
           backgroundAlt = colors.black;
           highlight = {
             background = colors.blue;
             foreground = colors.white;
           };
         };
         active = {
           background = colors.background;
           foreground = colors.foreground;
           backgroundAlt = colors.black;
           highlight = {
             background = colors.blue;
             foreground = colors.white;
           };
         };
         urgent = {
           background = colors.background;
           foreground = colors.foreground;
           backgroundAlt = colors.black;
           highlight = {
             background = colors.blue;
             foreground = colors.white;
           };
         };
        };
      };
    };
  };
}
