pkgs: colors:
let
  openUrlCmd = pkgs.writeScript "openUrlCmd" ''
      ${pkgs.xurls}/bin/xurls | ${pkgs.rofi}/bin/rofi -dmenu | xargs -r ${pkgs.firefox}/bin/firefox
    '';
in
  pkgs.st.overrideDerivation (old: {
    patches = old.patches ++ [
          # scrollback patches from https://st.suckless.org/patches/scrollback/
          # https://st.suckless.org/patches/scrollback/st-scrollback-0.7.diff
          ./st-alpha-0.7.diff
          ./st-externalpipe-0.7.diff
    ];
    enableParallelBuilds = true;
    prePatch = ''
      cp ${(pkgs.substituteAll { src = ./config.h; inherit openUrlCmd; inherit (colors) black brightBlack red brightRed green brightGreen yellow brightYellow blue brightBlue magenta brightMagenta cyan brightCyan white brightWhite background foreground;})} config.h
    '';
  })
