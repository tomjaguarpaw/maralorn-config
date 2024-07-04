{ pkgs, lib, ... }:
{
  home.sessionVariables.TERMINAL = lib.getExe pkgs.kitty;
  programs.kitty = {
    enable = true;
    keybindings = {
      "ctrl+plus" = "change_font_size all +0.25";
      "ctrl+minus" = "change_font_size all -0.25";
    };
    extraConfig = ''
      modify_font cell_height -2px
      modify_font baseline +2px
      font_size 8.75
      font_family JetBrainsMono NF SemiBold
      bold_font JetBrainsMono NF ExtraBold
      italic_font JetBrainsMono NF SemiBold Italic
      bold_italic_font JetBrainsMono NF ExtraBold Italic
    '';
  };
}
