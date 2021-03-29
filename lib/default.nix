rec {
  themes = rec {
    default = papercolor-light;
    papercolor-light = rec {
      primary = {
        foreground = normal.black;
        background = normal.white;
      };
      normal = {
        white = "#eeeeee";
        red = "#af0000";
        green = "#008700";
        yellow = "#5f8700";
        blue = "#0087af";
        magenta = "#878787";
        cyan = "#005f87";
        black = "#444444";
      };

      bright = {
        white = "#bcbcbc";
        red = "#d70000";
        green = "#d70087";
        yellow = "#8700af";
        blue = "#d75f00";
        magenta = "#d75f00";
        cyan = "#005faf";
        black = "#005f87";
      };
    };
    solarized-light = {
      primary = {
        foreground = "#586e75";
        background = "#fdf6e3";
      };
      normal = {
        black = "#073642";
        red = "#dc322f";
        green = "#859900";
        yellow = "#b58900";
        blue = "#268bd2";
        magenta = "#d33682";
        cyan = "#2aa198";
        white = "#eee8d5";
      };

      bright = {
        black = "#002b36";
        red = "#cb4b16";
        green = "#586e75";
        yellow = "#657b83";
        blue = "#839496";
        magenta = "#6c71c4";
        cyan = "#93a1a1";
        white = "#fdf6e3";
      };
    };
    maralorn-dark = {
      primary = {
        foreground = "#dddbff";
        background = "#000018";
      };
      normal = {
        black = "#000000";
        red = "#e34b4f";
        green = "#67b779";
        yellow = "#ff9c00";
        blue = "#5c67ff";
        magenta = "#cb85ff";
        cyan = "#17d0f4";
        white = "#dddbff";
      };
      bright = {
        black = "#55508f";
        red = "#e34b4f";
        green = "#45b75e";
        yellow = "#ff9c00";
        blue = "#5c67ff";
        magenta = "#cb85ff";
        cyan = "#17d0f4";
        white = "#ffffff";
      };
    };
  };

}
