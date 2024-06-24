{ config, pkgs, ... }:
{
  home.packages = [
    pkgs.hyprnome
    pkgs.hyprdim
  ];
  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [
      #pkgs.hyprscroller
      pkgs.hyprslidr
      pkgs.hyprlandPlugins.hyprexpo
    ];
    settings = {
      "$mod" = "SUPER";
      input = {
        kb_layout = "de";
        kb_variant = "neo";
        kb_options = "altwin:swap_lalt_lwin";
      };
      general = {
        gaps_in = 1;
        gaps_out = 1;
        "col.inactive_border" = "0x00000000";
        layout = "slidr";
        animation = [
          "windows,1,3,default"
          "fade,1,3,default"
          "border,1,10,default"
          "borderangle,1,10,default"
          "workspaces,1,2,default,slidevert"
        ];
      };
      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
      };
      windowrulev2 = [
        "float,class:launcher"
        "pin,class:launcher"
      ];
      exec = [
        "systemctl --user restart wallpaper eww swayidle"
        "unlock-keys"
        "hyprdim"
      ];
      bind = [
        "$mod, RETURN, exec, ${config.home.sessionVariables.TERMINAL}"
        "$mod, space, exec, ${config.home.sessionVariables.TERMINAL} -oapp-id=launcher my-hotkeys"
        "$mod, q, killactive"
        "$mod, left, slidr:movefocus, l"
        "$mod, right, slidr:movefocus, r"
        "$mod, up, slidr:movefocus, u"
        "$mod, down, slidr:movefocus, d"
        "SUPER_SHIFT, left, movewindow, l"
        "SUPER_SHIFT, right, movewindow, r"
        "SUPER_SHIFT, up, movewindow, u"
        "SUPER_SHIFT, down, movewindow, d"
        "$mod, f, fullscreen"
        "$mod, s, slidr:cyclesize"
        "$mod, i, slidr:admitwindow"
        "$mod, e, slidr:expelwindow"
        "$mod, t, togglefloating"
        "$mod, o, hyprexpo:expo, toggle"
        "$mod, Prior, exec, hyprnome --previous"
        "$mod, Next, exec, hyprnome"
        "SUPER_SHIFT, Prior, exec, hyprnome --previous --move"
        "SUPER_SHIFT, Next, exec, hyprnome --move"
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
      monitor = [
        ",highres,auto,1"
        "HDMI-A-1,highres,auto,1,mirror,DP-3"
      ];
    };
  };
}
