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
        monospace = ["JetBrainsMono Nerd Font" "Noto Sans Mono"] ++ unicode-fallback;
        sansSerif = ["B612" "Noto Sans"] ++ unicode-fallback;
        serif = ["Libertinus Serif" "Noto Serif"] ++ unicode-fallback;
      };
    };
    fonts = builtins.attrValues {
      inherit
        (pkgs)
        nerdfonts # For all my terminal needs.
        
        libertinus # nice text font
        
        material-icons # icons in my app
        
        b612 # sans font, very good for displays
        
        noto-fonts # for unicode fallback
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
