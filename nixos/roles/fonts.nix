{
  config,
  pkgs,
  lib,
  ...
}: {
  fonts = {
    fontconfig = {
      enable = true;
      cache32Bit = true;
      defaultFonts = let
        unicode-fallback = ["Noto Sans Symbols" "Noto Sans Symbols2"];
      in {
        monospace = ["Spleen 6x12" "Noto Sans Mono"] ++ unicode-fallback;
        sansSerif = ["B612" "Noto Sans"] ++ unicode-fallback;
        serif = ["Libertinus Serif" "Noto Serif"] ++ unicode-fallback;
      };
    };
    fonts = builtins.attrValues {
      inherit
        (pkgs)
        libertinus
        # nice text font
        
        material-icons
        # icons in my app
        
        tamzen
        # 12px
        
        cozette
        # 13px
        
        # too wide: dina-font
        
        # can‘t find font in there: efont-unicode
        
        # too wide: envypn-font
        
        spleen
        # Great if you need 8 px font, also nice on 12px.
        
        gohufont
        tewi-font
        # Too wide tracking: curie
        
        scientifica
        # Quite cool on: 11px
        
        # Tried but didn‘t like:
        
        # biwidth: too small
        
        # Too wide tracking: creep
        
        #
        
        # For all my terminal needs.
        
        b612
        # sans font, very good for displays
        
        noto-fonts
        # for unicode fallback
        
        ;
    };
  };

  # create a cache of the font sources, often slow internet connections make it painful to
  # re-download them after a few months
  environment.etc = let
    # fonts with src attributes
    font_sources = map (v: v.src) (lib.filter (v: v ? src) config.fonts.fonts);
  in
    builtins.listToAttrs (lib.imap0
      (n: source:
        lib.nameValuePair "src-cache/fonts/${toString n}" {
          inherit source;
        })
      font_sources);
}
