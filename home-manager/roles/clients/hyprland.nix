{
  config,
  pkgs,
  lib,
  ...
}:
let
  toggle-overlay = pkgs.writeShellScript "toggle-overlay" ''
    if [[ "$(wlr-randr --json | jq 'map(select(.enabled)) | length')" == "2" ]]; then
      (makoctl mode -r show; eww close overlay-0 overlay-1) || (eww open-many overlay-0 overlay-1; makoctl mode -a show)
    else
    (makoctl mode -r show; eww close overlay-0) || (eww open overlay-0; makoctl mode -a show)
    fi
  '';
in
{
  home.packages = builtins.attrValues { inherit (pkgs) wlr-randr; };
  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [ pkgs.hyprlandPlugins.hyprscroller ];
    settings = {
      "$mod" = "SUPER";
      input = {
        kb_layout = "de";
        kb_variant = "neo";
        kb_options = "altwin:swap_lalt_lwin";
        follow_mouse = 2;
      };
      decoration = {
        blur.enabled = false;
        shadow.enabled = false;
      };
      general = {
        border_size = 2;
        gaps_in = 0;
        gaps_out = 0;
        layout = "scroller";
        "col.active_border" = "0xff1e66f5";
        "col.inactive_border" = "0xffccced7";
        animation = [
          "global,0"
          "windows,1,1,default"
          "workspaces,1,1,default,slidevert"
        ];
      };
      plugin.scroller = {
        column_default_width = "onefourth";
        focus_wrap = false;
        column_widths = "onefourth onethird onehalf twothirds one";
      };
      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
      };
      windowrulev2 = [
        "float,class:launcher"
        "pin,class:launcher"
      ];
      exec = [ "systemctl --user restart eww swayidle" ];
      exec-once = [
        (lib.getExe pkgs.emote)
        "unlock-keys"
        (lib.getExe pkgs.hyprdim)
        "${lib.getExe pkgs.swaybg} -m fill -i ~/.config/wallpaper"
      ];
      bind = [
        "$mod, RETURN, exec, ${config.home.sessionVariables.TERMINAL}"
        "$mod, space, exec, ${config.home.sessionVariables.TERMINAL} --app-id=launcher my-hotkeys"
        "$mod, q, killactive"
        "$mod, p, pin"
        "$mod, m, focusmonitor, +1"
        "SUPER_SHIFT, m, movecurrentworkspacetomonitor, +1"
        "$mod, left, scroller:movefocus, l"
        "$mod, right, scroller:movefocus, r"
        "$mod, up, scroller:movefocus, u"
        "$mod, down, scroller:movefocus, d"
        "SUPER_SHIFT, left, scroller:movewindow, l"
        "SUPER_SHIFT, right, scroller:movewindow, r"
        "SUPER_SHIFT, up, scroller:movewindow, u"
        "SUPER_SHIFT, down, scroller:movewindow, d"
        "$mod, f, fullscreen"
        "$mod, c, exec, hyprctl keyword general:col.active_border 0xffff00ff"
        "$mod, c, scroller:setmode, col"
        "$mod, i, scroller:fitsize, all"
        "$mod, r, exec, hyprctl keyword general:col.active_border 0xff1e66f5"
        "$mod, r, scroller:setmode, row"
        "$mod, s, scroller:cyclesize, next"
        "$mod, z, scroller:alignwindow, c"
        "$mod, a, scroller:admitwindow"
        "$mod, e, scroller:expelwindow"
        "$mod, o, scroller:toggleoverview"
        "$mod, t, togglefloating"
        "$mod, Prior, exec, ${lib.getExe pkgs.hyprnome} --previous"
        "$mod, Next, exec, ${lib.getExe pkgs.hyprnome}"
        "SUPER_SHIFT, Prior, exec, ${lib.getExe pkgs.hyprnome} --previous --move"
        "SUPER_SHIFT, Next, exec, ${lib.getExe pkgs.hyprnome} --move"
        ", Print, execr, screenshot"
      ];
      monitor = [ ",highres,auto,1" ];
      bindr = [ "SUPER, SUPER_L, execr, ${toggle-overlay}" ];
      binde = [
        "SUPER_ALT, Left, resizeactive, -10 0"
        "SUPER_ALT, Right, resizeactive, 10 0"
        "SUPER_ALT, Up, resizeactive, 0 -10"
        "SUPER_ALT, Down, resizeactive, 0 10"
      ];
      bindm = [
        "$mod,mouse:272,movewindow"
        "$mod,mouse:273,resizewindow"
      ];
    };
  };
}
