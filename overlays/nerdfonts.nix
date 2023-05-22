self: super: {
  nerdfonts = super.nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; };
}
