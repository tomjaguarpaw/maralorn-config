{ pkgs, lib, ... }:
let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  services.gpg-agent.pinentryFlavor = "gnome3";
  dconf.settings = {
    "org/gnome/desktop/wm/keybindings" = {
      switch-input-source = [ ];
      switch-input-source-backward = [ ];
    };

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-timeout = 10800;
      sleep-inactive-ac-type = "suspend";
    };

    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Super>q" ];
    };

    "org/gnome/shell/extensions/gtile" = {
      global-presets = true;
      grid-sizes = "6x2";
      preset-resize-1 = [ "<Super>KP_1" ];
      preset-resize-2 = [ "<Super>KP_2" ];
      preset-resize-3 = [ "<Super>KP_3" ];
      preset-resize-4 = [ "<Super>KP_4" ];
      preset-resize-5 = [ "<Super>KP_5" ];
      preset-resize-6 = [ "<Super>KP_6" ];
      preset-resize-7 = [ "<Super>KP_7" ];
      preset-resize-8 = [ "<Super>KP_8" ];
      preset-resize-9 = [ "<Super>KP_9" ];
      resize1 = "6x2 0:1 1:1,0:1 2:1,0:1 3:1,0:1 0:1";
      resize2 = "6x2 2:1 3:1, 1:1 4:1";
      resize3 = "6x2 4:1 5:1,3:1 5:1,2:1 5:1,5:1 5:1";
      resize4 = "6x1 0:0 1:0,0:0 2:0,0:0 3:0,0:0 0:0";
      resize5 = "6x1 2:0 3:0, 1:0 4:0";
      resize6 = "6x1 4:0 5:0,3:0 5:0,2:0 5:0,5:0 5:0";
      resize7 = "6x2 0:0 1:0,0:0 2:0,0:0 3:0,0:0 0:0";
      resize8 = "6x2 2:0 3:0, 1:0 4:0";
      resize9 = "6x2 4:0 5:0,3:0 5:0,2:0 5:0,5:0 5:0";
      show-toggle-tiling-alt = [ "<Super>t" ];
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
    };

    "org/gnome/desktop/calendar" = {
      show-weekdate = true;
    };

    "org/gnome/desktop/wm/preferences" = {
      titlebar-font = "B612 9";
    };

    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "gTile@vibou"
        "clipboard-indicator@tudmotu.com"
        "appindicatorsupport@rgcjonas.gmail.com"
        "nothing-to-say@extensions.gnome.wouter.bolsterl.ee"
        "notification-position@drugo.dev"
        "drive-menu@gnome-shell-extensions.gcampax.github.com"
        "sound-output-device-chooser@kgshank.net"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "caffeine@patapon.info"
        "dash-to-panel@jderose9.github.com"
        "system-monitor@paradoxxx.zero.gmail.com"
      ];
      welcome-dialog-last-shown-version = "40.1";
    };

    "org/gnome/shell/extensions/dash-to-panel" = {
      animate-app-switch = false;
      animate-window-launch = false;
      appicon-margin = 0;
      appicon-padding = 4;
      group-apps = false;
      group-apps-label-font-color = "#613583";
      group-apps-label-font-color-minimized = "#1a5fb4";
      group-apps-label-font-size = 13;
      group-apps-label-font-weight = "inherit";
      group-apps-underline-unfocused = false;
      group-apps-use-fixed-width = false;
      group-apps-use-launchers = false;
      isolate-monitors = false;
      isolate-workspaces = true;
      leftbox-padding = -1;
      overview-click-to-exit = false;
      panel-element-positions = ''{"0":[{"element":"showAppsButton","visible":false,"position":"stackedTL"},{"element":"taskbar","visible":true,"position":"stackedTL"},{"element":"dateMenu","visible":true,"position":"centerMonitor"},{"element":"leftBox","visible":true,"position":"stackedTL"},{"element":"activitiesButton","visible":false,"position":"stackedTL"},{"element":"centerBox","visible":true,"position":"stackedBR"},{"element":"rightBox","visible":true,"position":"stackedBR"},{"element":"systemMenu","visible":true,"position":"stackedBR"},{"element":"desktopButton","visible":false,"position":"stackedBR"}]," 0 ":[{" element ":" showAppsButton "," visible ":false," position ":" stackedTL "},{" element ":" activitiesButton "," visible ":false," position ":" stackedTL "},{" element ":" dateMenu "," visible ":true," position ":" stackedTL "},{" element ":" leftBox "," visible ":true," position ":" stackedTL "},{" element ":" taskbar "," visible ":true," position ":" stackedTL "},{" element ":" rightBox "," visible ":true," position ":" stackedBR "},{" element ":" centerBox "," visible ":true," position ":" stackedTL "},{" element ":" systemMenu "," visible ":true," position ":" stackedBR "},{" element ":" desktopButton "," visible ":true," position ":" stackedBR "}]}'';
      panel-positions = ''{"0":"TOP"}'';
      panel-sizes = ''{"0":32}'';
      show-appmenu = true;
      show-favorites = false;
      show-running-apps = true;
      status-icon-padding = -1;
      trans-panel-opacity = 0.8;
      trans-use-custom-opacity = true;
      tray-padding = -1;
    };

    "org/gnome/shell/extensions/user-theme" = {
      name = "Arc-Lighter";
    };

    "system/locale" = {
      region = "en_DK.UTF-8";
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
      sources = [ (mkTuple [ "xkb" "de+neo" ]) ]; # use neo
      xkb-options = [
        "altwin:swap_lalt_lwin" # swap alt and win
        "lv3:menu_switch" # So that gnome-settings does not set it to ralt
      ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal" =
      {
        binding = "<Super>Return";
        command = "kitty mytmux";
        name = "Terminal";
      };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/hotkeys" =
      {
        binding = "<Super>space";
        command = "kitty hotkeys";
        name = "Hotkeys";
      };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/clear-notifications" =
      {
        binding = "<Super>r";
        command = "dbus-send --session --type=method_call --dest=org.gnome.Shell /org/gnome/Shell org.gnome.Shell.Eval string:'Main.panel.statusArea.dateMenu._messageList._sectionList.get_children().forEach(s => s.clear());'";
        name = "Clear Notifications";
      };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/standby" = {
      binding = "<Super>F5";
      command = "systemctl suspend";
      name = "Standby";
    };

    "org/gnome/shell/extensions/nothing-to-say" = {
      icon-visibility = "always";
      keybinding-toggle-mute = [ "<Primary><Shift>U+2113" ]; # Mouse key side middle
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/hotkeys/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/standby/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/clear-notifications/"
      ];
      cycle-windows = [ "<Super>Tab" ];
      cycle-windows-backward = [ "<Shift><Super>Tab" ];
      next = [ "<Primary><Shift>dollar" ];
      play = [ "<Primary><Shift>guillemotleft" ];
      previous = [ "<Primary><Shift>EuroSign" ];
      screensaver = [ "<Primary>Escape" ];
      volume-down = [ "<Primary><Shift>section" ];
      volume-up = [ "<Primary><Shift>degree" ];
      area-screenshot-clip = [ "Print" ];
      screenshot = [ ];
    };
  };
}
