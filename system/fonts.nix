{ config, pkgs, lib, ... }:

{
  fonts = {
    fontconfig = {
      enable = true;
      antialias = true;
      cache32Bit = true;
      defaultFonts = {
        monospace = [ "Source Code Pro For Powerline" "Roboto Mono" "DejaVu Sans Mono" ];
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
    fonts = [
      pkgs.anonymousPro
      pkgs.arkpandora_ttf
      pkgs.caladea
      pkgs.carlito
      pkgs.comfortaa
      pkgs.comic-relief
      pkgs.crimson
      pkgs.dejavu_fonts
      pkgs.google-fonts
      pkgs.inconsolata
      pkgs.iosevka
      pkgs.liberationsansnarrow
      pkgs.liberation_ttf
      pkgs.libertine
      pkgs.mononoki
      pkgs.montserrat
      pkgs.nerdfonts
      pkgs.norwester-font
      pkgs.opensans-ttf
      pkgs.powerline-fonts
      pkgs.roboto
      pkgs.sampradaya
      pkgs.source-code-pro
      pkgs.source-sans-pro
      pkgs.source-serif-pro
      pkgs.tai-ahom
      pkgs.tempora_lgc
      pkgs.terminus_font
      pkgs.theano
      pkgs.ubuntu_font_family
    ];
  };


  # create a cache of the font sources, often slow internet connections make it painful to
  # re-download them after a few months
  environment.etc = let
    # fonts with src attributes
    font_sources = map (v: v.src) (lib.filter (v: v ? src) config.fonts.fonts);
  in builtins.listToAttrs (lib.imap0 (n: v: lib.nameValuePair "src-cache/fonts/${toString n}" { source = builtins.toPath v; }) font_sources);
}
