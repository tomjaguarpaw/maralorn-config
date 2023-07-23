{
  pkgs,
  config,
  lib,
  ...
}@args:
let
  hotkeys = pkgs.writeShellScriptBin "my-hotkeys" ''
    ${pkgs.wizards-dialog}/bin/hotkeys ${
      pkgs.writeText "hotkeys.yaml" (builtins.toJSON (import ./_hotkeys.nix args))
    }
  '';
in
{
  home.packages = [ hotkeys ];
  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      input = {
        kb_layout = "de";
        kb_variant = "neo";
        kb_options = "altwin:swap_lalt_lwin"; # swap alt and win
      };
      general = {
        gaps_in = 3;
        gaps_out = 50;
      };
      decoration.blur = false;
      "$mod" = "SUPER";
      windowrulev2 = [ "float,class:launcher" ];
      exec = [ "hyprpaper" ];
      bind = [
        "$mod, RETURN, exec, ${config.home.sessionVariables.TERMINAL}"
        "$mod, space, exec, ${config.home.sessionVariables.TERMINAL} -oapp-id=launcher ${
          lib.getExe hotkeys
        }"
        "$mod, q, killactive"
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"
        "SUPER_SHIFT, left, movewindow, l"
        "SUPER_SHIFT, right, movewindow, r"
        "SUPER_SHIFT, up, movewindow, u"
        "SUPER_SHIFT, down, movewindow, d"
        "$mod, f, fullscreen"
        "$mod, t, togglefloating"
      ];

      monitor = [ ",preferred,auto,1" ];
    };
  };
}
