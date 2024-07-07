{
  config,
  pkgs,
  lib,
  ...
}:
{
  home.packages = builtins.attrValues { inherit (pkgs) wlr-randr kanshi; };
  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [ pkgs.hyprslidr ];
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
        drop_shadow = false;
      };
      general = {
        border_size = 1;
        layout = "slidr";
        "col.active_border" = "0xff1e66f5";
        "col.inactive_border" = "0xffccced7";
        animation = [
          "global,0"
          "windows,1,1,default"
          "workspaces,1,1,default,slidevert"
        ];
      };
      plugin.overview = {
        hideRealLayers = false;
        affectStrut = false;
        onBottom = true;
      };
      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
      };
      windowrulev2 = [
        "float,class:launcher"
        "pin,class:launcher"
      ];
      exec = [ "systemctl --user restart eww swayidle kanshi" ];
      exec-once = [
        "unlock-keys"
        (lib.getExe pkgs.hyprdim)
        "${lib.getExe pkgs.swaybg} -m fill -i ~/.config/wallpaper"
      ];
      bind = [
        "$mod, RETURN, exec, ${config.home.sessionVariables.TERMINAL}"
        "$mod, space, exec, ${config.home.sessionVariables.TERMINAL} --app-id=launcher my-hotkeys"
        "$mod, q, killactive"
        "$mod, left, slidr:movefocus, l"
        "$mod, right, slidr:movefocus, r"
        "$mod, up, slidr:movefocus, u"
        "$mod, down, slidr:movefocus, d"
        "SUPER_SHIFT, left, slidr:movewindow, l"
        "SUPER_SHIFT, right, slidr:movewindow, r"
        "SUPER_SHIFT, up, slidr:movewindow, u"
        "SUPER_SHIFT, down, slidr:movewindow, d"
        "$mod, f, fullscreen"
        "$mod, s, slidr:cyclesize"
        "$mod, a, slidr:admitwindow"
        "$mod, e, slidr:expelwindow"
        "$mod, t, togglefloating"
        "$mod, Prior, exec, ${lib.getExe pkgs.hyprnome} --previous"
        "$mod, Next, exec, ${lib.getExe pkgs.hyprnome}"
        "SUPER_SHIFT, Prior, exec, ${lib.getExe pkgs.hyprnome} --previous --move"
        "SUPER_SHIFT, Next, exec, ${lib.getExe pkgs.hyprnome} --move"
        ", Print, execr, screenshot"
      ];
      bindr = [
        "SUPER, SUPER_L, execr, (makoctl mode -r show; eww close overlay) || (eww open overlay; makoctl mode -a show)"
      ];
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
