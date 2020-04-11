{ pkgs, ... }:
let
  my-lib = import ../../lib;
  inherit (my-lib) colors;
in {
  xdg.configFile."kitty/kitty.conf".text = ''
    font_size 10.5

    foreground ${colors.foreground}
    background ${colors.background}
    background_opacity 0.9

    selection_foreground ${colors.background}
    selection_background ${colors.foreground}

    color0 ${colors.black}
    color8 ${colors.brightBlack}
    #: black

    color1 ${colors.red}
    color9 ${colors.brightRed}
    #: red

    color2  ${colors.green}
    color10 ${colors.brightGreen}
    #: green

    color3  ${colors.yellow}
    color11 ${colors.brightYellow}
    #: yellow

    color4  ${colors.blue}
    color12 ${colors.brightBlue}
    #: blue

    color5  ${colors.magenta}
    color13 ${colors.brightMagenta}
    #: magenta

    color6  ${colors.cyan}
    color14 ${colors.brightCyan}
    #: cyan

    color7  ${colors.white}
    color15 ${colors.brightWhite}

    term xterm-256color

    map ctrl+shift+y paste_from_clipboard
    map ctrl+y paste_from_selection
  '';
}
