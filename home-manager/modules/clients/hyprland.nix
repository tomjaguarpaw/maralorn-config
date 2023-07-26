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
  home.packages = [
    hotkeys
    pkgs.slurp
    pkgs.grim
  ];
  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      input = {
        kb_layout = "de";
        kb_variant = "neo";
        kb_options = "altwin:swap_lalt_lwin"; # swap alt and win
      };
      general = {
        gaps_in = 2;
        gaps_out = 4;
        "col.inactive_border" = "0x00000000";
      };
      decoration.blur = false;
      "$mod" = "SUPER";
      windowrulev2 = [
        "float,class:launcher"
        "nofocus,class:Conky"
        "noborder,class:Conky"
        "pin,class:Conky"
      ];
      exec-once = [
        (lib.getExe pkgs.hyprpaper)
        "unlock-ssh"
        (lib.getExe pkgs.kassandra)
        (pkgs.writeShellScript "wallpaper" ''
          sleep 1s
          ${lib.getExe pkgs.randomWallpaper}
        '')
      ];
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
        "$mod, g, togglegroup"
        "$mod, p, pin"
        "$mod, Tab, changegroupactive"
        "$mod, Prior, workspace, e-1"
        "$mod, Next, workspace, e+1"
        "SUPER_SHIFT, Prior, movetoworkspace, -1"
        "SUPER_SHIFT, Next, movetoworkspace, +1"
        "SUPER_ALT, Left, resizeactive, -10 0"
        "SUPER_ALT, Right, resizeactive, 10 0"
        "SUPER_ALT, Up, resizeactive, 0 -10"
        "SUPER_ALT, Down, resizeactive, 0 10"
        "$mod, Print, execr, grim -g $(slurp)"
      ];
      bindm = [
        "$mod,mouse:272,movewindow"
        "$mod,mouse:273,resizewindow"
      ];
      monitor = [ ",preferred,auto,1" ];
    };
  };
}
