{
  fonts.fontDir.enable = true; # So that connect can find fonts.
  programs.xwayland.defaultFontPath = ""; # So that the fontDir option does not trigger regular rebuilds of xwayland.
}
