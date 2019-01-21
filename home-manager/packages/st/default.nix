pkgs: colors:
let
  openUrlCmd = pkgs.writeScript "openUrlCmd" ''
      ${pkgs.xurls}/bin/xurls | ${pkgs.rofi}/bin/rofi -dmenu | xargs -r ${pkgs.firefox}/bin/firefox
    '';
in
  pkgs.st.overrideDerivation (old: {
    patches = old.patches ++ [
          ./st-externalpipe-0.8.1.diff
          ./st-alpha-0.8.1.diff
    ];
    enableParallelBuilds = true;
    prePatch = ''
      cp ${(pkgs.substituteAll { src = ./config.h; inherit openUrlCmd; inherit (colors) black brightBlack red brightRed green brightGreen yellow brightYellow blue brightBlue magenta brightMagenta cyan brightCyan white brightWhite background foreground;})} config.h
    '';
  })
