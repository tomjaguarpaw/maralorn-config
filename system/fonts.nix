{ config, pkgs, lib, ... }:

{
  fonts = {
    fontconfig = {
      enable = true;
      antialias = true;
      cache32Bit = true;
      defaultFonts = {
        monospace =
          [ "Source Code Pro For Powerline" "Roboto Mono" "DejaVu Sans Mono" ];
        sansSerif = [ "Roboto Regular" "DejaVu Sans" ];
        serif = [ "Roboto Slab Regular" "DejaVu Serif" ];
      };
      ultimate = {
        enable = true;
        substitutions = "combi";
      };
    };
    enableDefaultFonts = true;
    enableFontDir = true;
    fonts = builtins.attrValues {
      inherit (pkgs)
        anonymousPro arkpandora_ttf caladea carlito comfortaa comic-relief
        crimson dejavu_fonts google-fonts inconsolata iosevka
        liberationsansnarrow liberation_ttf libertine mononoki montserrat
        nerdfonts norwester-font opensans-ttf powerline-fonts roboto sampradaya
        source-code-pro source-sans-pro source-serif-pro tai-ahom tempora_lgc
        terminus_font theano ubuntu_font_family hasklig;
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
