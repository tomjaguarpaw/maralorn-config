{ pkgs, lib, config, ... }:
let
  workspaces = config.common.workspaces;
  terminal = config.common.terminal;
  colors = config.common.colors;
  rofiScriptWeb = pkgs.writeShellScriptBin "rofi-script-web" ''
    if [[ -z $@ ]]; then
       sed 's/^[0-9]*\(-r\)\? \?//;s/^\([^[:space:]]*\).*$/\1/' $HOME/.local/share/qutebrowser/history | tac
    else
       ${pkgs.qutebrowser}/bin/qutebrowser "$1" > /dev/null &
    fi
  '';
  rofiScriptI3 = pkgs.writeShellScriptBin "rofi-script-i3" ''
    if [ -z $@ ]; then
      (i3-msg -t get_workspaces | tr ',' '\n' | grep "name" | sed 's/"name":"\(.*\)"/\1/g';
      echo "${builtins.concatStringsSep "\n" (builtins.foldl' (labels: name: let
            number = toString (builtins.length labels);
          in
          labels ++ [ "${number}:${name}" ]
          ) [] workspaces)}") | sort -u
    else
      i3-msg workspace "$@" >/dev/null
    fi
  '';
  rofiTask = pkgs.writeScriptBin "tasklauncher" (builtins.readFile ./tasklauncher.py);
#  recollPython = pkgs.python2.withPackages (ps: [
#    pkgs.recoll
#    ]);
#  rofiFind = pkgs.writeScriptBin "zzzfoo" (builtins.replaceStrings [ "/usr/bin/env python" ] [ "${recollPython}/bin/python" ] (builtins.readFile ./zzzfoo/zzzfoo));
in {
  home = {
    packages = with pkgs; [
      rofi
      rofiScriptWeb
      rofiScriptI3
      rofiTask
#      rofiFind
      rofi-pass
#      recoll
    ];
  };
  programs = {
    rofi = {
      enable = true;
      extraConfig = ''
        rofi.modi: combi,window,drun,run,ssh,keys,web:rofi-script-web,i3:rofi-script-i3
        rofi.sidebar-mode: true
        rofi.combi-modi: window,drun,run
        '';
      separator = "solid";
      terminal = terminal;
      location = "left";
      scrollbar = false;
      width = 1500;
      yoffset = -10;
      lines = 35;
      colors = {
        window = {
          background = colors.background;
          border = colors.blue;
          separator = colors.blue;
        };
        rows = {
          normal = {
            background = colors.background;
            foreground = colors.foreground;
            backgroundAlt = colors.black;
            highlight = {
              background = colors.blue;
              foreground = colors.white;
            };
          };
        };
      };
    };
  };
}
