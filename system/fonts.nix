{ config, pkgs, lib, ... }:
let inherit (import ../lib) unstable;
in {
  fonts = {
    fontconfig = {
      enable = true;
      cache32Bit = true;
      defaultFonts = {
        monospace = [ "Jetbrains Mono" "DejaVu Sans Mono" ];
        sansSerif = [ "B612" "DejaVu Sans" ];
        serif = [ "Roboto Slab Regular" "DejaVu Serif" ];
      };
    };
    enableDefaultFonts = true;
    enableFontDir = true;
    fonts = builtins.attrValues {
      inherit (pkgs)
        nerdfonts # emojis
        libertine # because I like them
        roboto # serif font
        font-awesome # icons I guess?
        material-icons # icons in my app
        b612 # sans font
        powerline-fonts # fonts e.g. for swaybar
      ;
      inherit (unstable) jetbrains-mono # code font
      ;
    };
  };

  # create a cache of the font sources, often slow internet connections make it painful to
  # re-download them after a few months
  environment.etc = let
    # fonts with src attributes
    font_sources = map (v: v.src) (lib.filter (v: v ? src) config.fonts.fonts);
  in builtins.listToAttrs (lib.imap0 (n: v:
    lib.nameValuePair "src-cache/fonts/${toString n}" {
      source = builtins.toPath v;
    }) font_sources);
}
