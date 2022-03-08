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
        nerdfonts
        # For all my terminal needs.
        libertinus
        # nice text font
        material-icons
        # icons in my app
        b612
        ; # sans font, very good for displays
    };
  };

  # create a cache of the font sources, often slow internet connections make it painful to
  # re-download them after a few months
  environment.etc = let
    # fonts with src attributes
    font_sources = map (v: v.src) (lib.filter (v: v ? src) config.fonts.fonts);
  in
    builtins.listToAttrs (lib.imap0
    (n: v:
      lib.nameValuePair "src-cache/fonts/${toString n}" {
        source = builtins.toPath v;
      })
    font_sources);
}
