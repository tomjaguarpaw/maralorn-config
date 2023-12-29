{ pkgs, ... }:
{
  fonts = {
    fontDir.enable = true;
    enableDefaultFonts = true;
    fontconfig = {
      enable = true;
      cache32Bit = true;
      defaultFonts =
        let
          unicode-fallback = [
            "Noto Sans Symbols"
            "Noto Sans Symbols2"
          ];
        in
        {
          monospace = [
            "Symbols Nerd Font Mono"
            "Spleen 6x12"
            "Noto Sans Mono"
          ] ++ unicode-fallback;
          sansSerif = [
            "B612"
            "Noto Sans"
          ] ++ unicode-fallback;
          serif = [
            "Libertinus Serif"
            "Noto Serif"
          ] ++ unicode-fallback;
        };
    };
    packages = builtins.attrValues {
      inherit (pkgs)
        libertinus # nice text font
        material-icons # icons in my app
        spleen # Great if you need 8 px font, also nice on 12px.
        b612 # sans font, very good for displays
        noto-fonts # for unicode fallback
        nerdfonts
        ;
    };
  };
}
