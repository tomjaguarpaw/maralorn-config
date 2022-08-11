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
      defaultFonts = {
        monospace = ["JetBrainsMono Nerd Font" "DejaVu Sans Mono"];
        sansSerif = ["B612" "DejaVu Sans"];
        serif = ["Libertinus" "DejaVu Serif"];
      };
    };
    enableDefaultFonts = true;
    #fontDir.enable = true;
    fonts = builtins.attrValues {
      inherit
        (pkgs)
        # For all my terminal needs.
        
        nerdfonts
        # nice text font
        
        libertinus
        # icons in my app
        
        material-icons
        # sans font, very good for displays
        
        b612
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
