{ pkgs, lib, ... }:
{
  home.sessionVariables.TERMINAL = lib.getExe pkgs.kitty;
  programs.kitty = {
    enable = true;
    keybindings = {
      "ctrl+plus" = "change_font_size all +0.25";
      "ctrl+minus" = "change_font_size all -0.25";
    };
    #modify_font cell_width -1px
    extraConfig = ''
      modify_font cell_height -2px
      modify_font baseline +2px
      font_size 7.75
      font_family JetBrainsMono NFM SemiBold
      bold_font JetBrainsMono NFM ExtraBold
      italic_font JetBrainsMono NFM SemiBold Italic
      bold_italic_font JetBrainsMono NFM ExtraBold Italic
    '';
  };

  #programs.foot = {
  #  enable = true;
  #  settings = {
  #    main.font = "Symbols Nerd Font Mono:pixelsize=11,Spleen:pixelsize=11";
  #    mouse.hide-when-typing = "yes";
  #    tweak.font-monospace-warn = "no";
  #  };
  #};
}
