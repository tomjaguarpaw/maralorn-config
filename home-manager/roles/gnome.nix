{
  pkgs,
  lib,
  ...
} @ args: let
  hotkeys = import ./hotkeys.nix args;
  extensions = builtins.attrValues {
    inherit
      (pkgs.gnomeExtensions)
      appindicator
      system-monitor
      clipboard-indicator
      window-is-ready-remover
      nothing-to-say
      windownavigator
      user-themes
      removable-drive-menu
      highlight-focus
      mouse-follows-focus
      gtile
      caffeine
      ;
    executor = pkgs.gnomeExtensions.executor.overrideAttrs (old: {
      postInstall =
        (old.postInstall or "")
        + ''
          substituteInPlace $out/share/gnome-shell/extensions/executor@raujonas.github.io/extension.js --replace "'/bin/bash'" "'bash'"
        '';
    });
  };
  inherit (lib.hm.gvariant) mkTuple;
in {
  home.packages = extensions;
  services.gpg-agent.pinentryFlavor = "gnome3";
  dconf.settings = {
    "org/gnome/desktop/wm/keybindings" = {
      switch-input-source = [];
      switch-input-source-backward = [];
      switch-applications = [];
      switch-applications-backward = [];
      cycle-windows = ["<Super>Tab"];
      cycle-windows-backward = ["<Shift><Super>Tab"];
      close = ["<Super>q"];
    };

    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
      night-light-schedule-automatic = false;
      night-light-schedule-from = 23.0;
    };

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-timeout = 900;
      sleep-inactive-ac-type = "suspend";
    };

    "org/gnome/shell/extensions/gtile" = let
      left = r: "1:${r} 2:${r},1:${r} 1:${r},2:${r} 2:${r}";
      right = r: "3:${r} 4:${r},4:${r} 4:${r},3:${r} 3:${r}";
      middle = r: "2:${r} 3:${r}, 2:${r} 2:${r}, 3:${r} 3:${r}, 1:${r} 4:${r}";
    in {
      global-presets = true;
      grid-sizes = "4x2";
      preset-resize-1 = ["<Control><Super>m"];
      preset-resize-2 = ["<Control><Super>comma"];
      preset-resize-3 = ["<Control><Super>period"];
      preset-resize-4 = ["<Control><Super>n"];
      preset-resize-5 = ["<Control><Super>r"];
      preset-resize-6 = ["<Control><Super>t"];
      preset-resize-7 = ["<Control><Super>h"];
      preset-resize-8 = ["<Control><Super>g"];
      preset-resize-9 = ["<Control><Super>f"];
      resize1 = "4x2 ${left "2"}";
      resize2 = "4x2 ${middle "2"}";
      resize3 = "4x2 ${right "2"}";
      resize4 = "4x1 ${left "1"}";
      resize5 = "4x1 ${middle "1"}";
      resize6 = "4x1 ${right "1"}";
      resize7 = "4x2 ${left "1"}";
      resize8 = "4x2 ${middle "1"}";
      resize9 = "4x2 ${right "1"}";
      show-toggle-tiling-alt = ["<Super>t"];
      show-icon = false;
    };

    "org/gnome/desktop/peripherals/mouse" = {
      speed = 1;
    };

    "org/gnome/desktop/interface" = {
      gtk-theme = "Arc";
      icon-theme = "Arc";
      document-font-name = "B612 9";
      font-antialiasing = "rgba";
      font-hinting = "slight";
      font-name = "B612 9";
      clock-show-weekday = true;
      monospace-font-name = "JetBrainsMono Nerd Font Mono Bold 9";
      locate-pointer = true;
    };

    "org/gnome/desktop/calendar" = {
      show-weekdate = true;
    };

    "org/gnome/desktop/wm/preferences" = {
      auto-raise = true;
      focus-mode = "sloppy";
      titlebar-font = "B612 9";
    };

    "org/gnome/shell/extensions/executor" = {
      location = 1;
      left-active = true;
      left-commands-json = ''{"commands":[{"command":"cat /run/user/1000/status-bar","interval":1,"uuid":"d20a15a4-aea9-48e1-955f-4bd9f55b08bc"}]}'';
      left-index = 0;
      center-active = false;
      right-active = false;
    };

    "org/gnome/shell" = {
      disable-extension-version-validation = true;
      disable-user-extensions = false;
      enabled-extensions = map (x: x.extensionUuid) extensions;
      welcome-dialog-last-shown-version = pkgs.gnome.gnome-shell.version;
    };

    "org/gnome/shell/extensions/user-theme" = {
      name = "Arc";
    };

    "system/locale" = {
      region = "en_DK.UTF-8";
    };

    "org/gnome/desktop/screensaver" = {
      lock-delay = "0"; # lock screen immediately on screen blank
    };

    "org/gnome/desktop/session" = {
      idle-delay = "300"; # blank screen after 5 minutes
    };

    "org/gnome/shell/extensions/system-monitor" = {
      center-display = true;
      compact-display = true;
      cpu-show-menu = false;
      cpu-show-text = false;
      cpu-style = "graph";
      icon-display = false;
      memory-show-text = false;
      memory-style = "graph";
      move-clock = false;
      net-show-menu = true;
      net-show-text = false;
      net-speed-in-bits = true;
      net-style = "both";
      show-tooltip = true;
    };

    "org/gnome/desktop/input-sources" = {
      sources = [(mkTuple ["xkb" "de+neo"])]; # use neo
      xkb-options = [
        "altwin:swap_lalt_lwin" # swap alt and win
        "lv3:menu_switch" # So that gnome-settings does not set it to ralt
      ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal" = {
      binding = "<Super>Return";
      command = "foot";
      name = "Terminal";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/hotkeys" = {
      binding = "<Super>space";
      command = "foot ${pkgs.wizards-dialog}/bin/hotkeys ${pkgs.writeText "hotkeys.yaml" (builtins.toJSON hotkeys)}";
      name = "Hotkeys";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/standby" = {
      binding = "<Super>F5";
      command = "systemctl suspend";
      name = "Standby";
    };

    "org/gnome/shell/extensions/nothing-to-say" = {
      icon-visibility = "always";
      keybinding-toggle-mute = ["<Primary><Shift>U+2113"]; # Mouse key side middle
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/hotkeys/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/standby/"
      ];
      next = ["<Primary><Shift>dollar"];
      play = ["<Primary><Shift>guillemotleft"];
      previous = ["<Primary><Shift>EuroSign"];
      screensaver = ["<Primary>Escape"];
      volume-down = ["<Primary><Shift>section"];
      volume-up = ["<Primary><Shift>degree"];
      area-screenshot-clip = ["Print"];
      screenshot = [];
    };
  };
}
