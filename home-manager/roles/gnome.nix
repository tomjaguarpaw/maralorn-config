{
  pkgs,
  lib,
  ...
} @ args: let
  hotkeys = import ./hotkeys.nix args;
  inherit (lib.hm.gvariant) mkTuple;
in {
  services.gpg-agent.pinentryFlavor = "gnome3";
  dconf.settings = {
    "org/gnome/desktop/wm/keybindings" = {
      switch-input-source = [];
      switch-input-source-backward = [];
      switch-applications = [];
      switch-applications-backward = [];
      cycle-windows = ["<Super>Tab"];
      cycle-windows-backward = ["<Shift><Super>Tab"];
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

    "org/gnome/desktop/wm/keybindings" = {
      close = ["<Super>q"];
    };

    "org/gnome/shell/extensions/gtile" = let
      left = r: "1:${r} 2:${r},1:${r} 3:${r},1:${r} 4:${r},1:${r} 1:${r}, 2:${r} 2:${r}";
      right = r: "5:${r} 6:${r},4:${r} 6:${r},3:${r} 6:${r},6:${r} 6:${r}, 5:${r} 5:${r}";
      middle = r: "3:${r} 4:${r}, 2:${r} 5:${r}, 1:${r} 6:${r}, 2:${r} 4:${r}, 3:${r} 5:${r}, 3:${r} 3:${r}, 4:${r} 4:${r}";
    in {
      global-presets = true;
      grid-sizes = "6x2";
      preset-resize-1 = ["<Control><Super>m"];
      preset-resize-2 = ["<Control><Super>comma"];
      preset-resize-3 = ["<Control><Super>period"];
      preset-resize-4 = ["<Control><Super>n"];
      preset-resize-5 = ["<Control><Super>r"];
      preset-resize-6 = ["<Control><Super>t"];
      preset-resize-7 = ["<Control><Super>h"];
      preset-resize-8 = ["<Control><Super>g"];
      preset-resize-9 = ["<Control><Super>f"];
      resize1 = "6x2 ${left "2"}";
      resize2 = "6x2 ${middle "2"}";
      resize3 = "6x2 ${right "2"}";
      resize4 = "6x1 ${left "1"}";
      resize5 = "6x1 ${middle "1"}";
      resize6 = "6x1 ${right "1"}";
      resize7 = "6x2 ${left "1"}";
      resize8 = "6x2 ${middle "1"}";
      resize9 = "6x2 ${right "1"}";
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
      titlebar-font = "B612 9";
    };

    "org/gnome/shell/extensions/executor" = {
      center-active = true;
      center-commands-json = ''{"commands":[{"command":"cat /run/user/1000/status-bar","interval":1,"uuid":"d20a15a4-aea9-48e1-955f-4bd9f55b08bc"}]}'';
      center-index = 0;
      left-active = false;
      location = 1;
      right-active = false;
    };

    "org/gnome/shell" = {
      disable-extension-version-validation = true;
      disable-user-extensions = false;
      enabled-extensions = [
        "gTile@vibou"
        "clipboard-indicator@tudmotu.com"
        "appindicatorsupport@rgcjonas.gmail.com"
        "nothing-to-say@extensions.gnome.wouter.bolsterl.ee"
        "drive-menu@gnome-shell-extensions.gcampax.github.com"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "caffeine@patapon.info"
        "dash-to-panel@jderose9.github.com"
        "system-monitor@paradoxxx.zero.gmail.com"
        "windowsNavigator@gnome-shell-extensions.gcampax.github.com"
        "executor@raujonas.github.io"
      ];
      welcome-dialog-last-shown-version = "40.1";
    };

    "org/gnome/shell/extensions/dash-to-panel" = {
      animate-app-switch = false;
      animate-window-launch = false;
      appicon-margin = 0;
      appicon-padding = 4;
      group-apps = false;
      isolate-monitors = false;
      isolate-workspaces = true;
      leftbox-padding = -1;
      overview-click-to-exit = false;
      panel-element-positions = ''{"0":[{"element":"showAppsButton","visible":false,"position":"stackedTL"},{"element":"taskbar","visible":false,"position":"stackedTL"},{"element":"dateMenu","visible":true,"position":"stackedTL"},{"element":"leftBox","visible":true,"position":"stackedTL"},{"element":"activitiesButton","visible":false,"position":"stackedTL"},{"element":"centerBox","visible":true,"position":"stackedBR"},{"element":"rightBox","visible":true,"position":"stackedBR"},{"element":"systemMenu","visible":true,"position":"stackedBR"},{"element":"desktopButton","visible":false,"position":"stackedBR"}]," 0 ":[{" element ":" showAppsButton "," visible ":false," position ":" stackedTL "},{" element ":" activitiesButton "," visible ":false," position ":" stackedTL "},{" element ":" dateMenu "," visible ":true," position ":" stackedTL "},{" element ":" leftBox "," visible ":true," position ":" stackedTL "},{" element ":" taskbar "," visible ":true," position ":" stackedTL "},{" element ":" rightBox "," visible ":true," position ":" stackedBR "},{" element ":" centerBox "," visible ":true," position ":" stackedTL "},{" element ":" systemMenu "," visible ":true," position ":" stackedBR "},{" element ":" desktopButton "," visible ":true," position ":" stackedBR "}]}'';
      panel-positions = ''{"0":"TOP"}'';
      panel-sizes = ''{"0":32}'';
      show-appmenu = false;
      show-favorites = false;
      show-running-apps = true;
      status-icon-padding = -1;
      tray-padding = -1;
      trans-panel-opacity = 0.0;
      tray-size = 12;
      leftbox-size = 12;
      trans-use-custom-opacity = true;
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
