{ pkgs, lib, config, ... }@args:
let
  hotkeys = pkgs.writeShellScriptBin "hotkeys" ''
    ${pkgs.wizards-dialog}/bin/hotkeys ${
      pkgs.writeText "hotkeys.yaml"
      (builtins.toJSON (import ./hotkeys.nix args))
    }
  '';
  extensions = builtins.attrValues {
    inherit (pkgs.gnomeExtensions)
      appindicator # For tray icons
      caffeine # This extension disables autoscreensaver on fullscreen and offers a manual toggle
      dash-to-panel # To move menu
      hide-minimized nothing-to-say # Microphone status tray icon
      notifications-to-file # For my statusbarscript
      pop-shell # For tiling
      removable-drive-menu # Show usb sticks
      user-themes # shell-theme
      window-is-ready-remover # less annoying notifications
      windownavigator # numbers for navigation in overview
    ;
  };
  inherit (lib.hm.gvariant) mkTuple mkUint32;
  font = "Monospace 9";
in {
  home.packages = extensions ++ [ hotkeys ];
  services.gpg-agent.pinentryFlavor = "gnome3";
  dconf.settings = {
    "org/gnome/desktop/notifications" = {
      show-banners = false;
      show-in-lock-screen = false;
    };
    "org/gnome/shell/keybindings" = { "toggle-overview" = [ ]; };
    "org/gnome/desktop/wm/keybindings" = {
      switch-input-source = [ ];
      switch-input-source-backward = [ ];
      switch-applications = [ ];
      switch-applications-backward = [ ];
      minimize = [ ];
      maximize = [ ];
      unmaximize = [ ];
      cycle-windows = [ "<Super>Tab" ];
      cycle-windows-backward = [ "<Shift><Super>Tab" ];
      close = [ "<Super>q" ];
      move-to-monitor-down = [ ];
      move-to-monitor-left = [ ];
      move-to-monitor-right = [ ];
      move-to-monitor-up = [ ];
      toggle-fullscreen = [ "<Super>f" ];
    };

    "org/gnome/mutter" = { dynamic-workspaces = true; };

    "org/gnome/mutter/keybindings" = {
      toggle-tiled-left = [ ];
      toggle-tiled-right = [ ];
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

    "org/gnome/desktop/peripherals/mouse" = { speed = 1; };

    "org/gnome/desktop/interface" = {
      document-font-name = font;
      font-name = font;
      monospace-font-name = font;
      font-antialiasing = "grayscale";
      font-hinting = "full";
      clock-show-weekday = true;
      clock-show-seconds = false;
      locate-pointer = true;
    };

    "org/gnome/desktop/calendar" = { show-weekdate = true; };

    "org/gnome/desktop/wm/preferences" = {
      auto-raise = true;
      titlebar-font = font;
    };

    "org/gnome/shell" = {
      disable-extension-version-validation = true;
      disable-user-extensions = false;
      enabled-extensions = map (x: x.extensionUuid) extensions;
      welcome-dialog-last-shown-version = pkgs.gnome.gnome-shell.version;
    };

    "org/gnome/shell/extensions/user-theme" = {
      name = "Catppuccin-Mocha-Compact-Blue-Dark";
    };

    "system/locale" = { region = "en_DK.UTF-8"; };

    "org/gnome/desktop/screensaver" = {
      lock-delay = "0"; # lock screen immediately on screen blank
    };

    "org/gnome/desktop/session" = {
      idle-delay = "300"; # blank screen after 5 minutes
    };

    "org/gnome/shell/extensions/pop-shell" = {
      gap-inner = mkUint32 1;
      gap-outer = mkUint32 1;
      active-hint-border-radius = mkUint32 0;
      smart-gaps = true;
      snap-to-grid = true;
      tile-by-default = true;
      active-hint = true;
      hint-color-rgba = "rgba(48, 0, 208,0.5)";
      tile-enter = [ "<Super>t" ];
      tile-move-left-global = [ "<Super><Shift>Left" ];
      tile-move-right-global = [ "<Super><Shift>Right" ];
      tile-move-up-global = [ "<Super><Shift>Up" ];
      tile-move-down-global = [ "<Super><Shift>Down" ];
      tile-resize-left = [ "n" ];
      tile-resize-right = [ "t" ];
      tile-resize-up = [ "g" ];
      tile-resize-down = [ "r" ];
      pop-workspace-up = [ ];
      pop-workspace-down = [ ];
      pop-monitor-left = [ ];
      pop-monitor-right = [ ];
      pop-monitor-up = [ ];
      pop-monitor-down = [ ];
    };

    "org/gnome/shell/extensions/dash-to-panel" = {
      panel-element-positions = ''
        {"0":[{"element":"showAppsButton","visible":false,"position":"stackedTL"},{"element":"dateMenu","visible":false,"position":"centerMonitor"},{"element":"activitiesButton","visible":false,"position":"stackedTL"},{"element":"taskbar","visible":true,"position":"stackedBR"},{"element":"leftBox","visible":false,"position":"stackedTL"},{"element":"centerBox","visible":false,"position":"stackedBR"},{"element":"rightBox","visible":true,"position":"stackedBR"},{"element":"systemMenu","visible":true,"position":"stackedBR"},{"element":"desktopButton","visible":false,"position":"stackedBR"}]}
      '';
      appicon-margin = 0;
      appicon-padding = 4;
      dot-position = "LEFT";
      panel-positions = ''{"0":"LEFT"}'';
      panel-anchors = ''{"0":"END"}'';
      panel-sizes = ''{"0":24}'';
      tray-padding = 2;
      show-favorites = false;
      show-running-apps = true;
      status-icon-padding = 2;
      leftbox-size = 13;
      hide-overview-on-startup = true;
      tray-size = 13;
      trans-use-custom-opacity = true;
      trans-panel-opacity = 0.0;
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
        command = "${config.home.sessionVariables.TERMINAL}";
        name = "Terminal";
      };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/hotkeys" =
      {
        binding = "<Super>space";
        command =
          "${config.home.sessionVariables.TERMINAL} ${lib.getExe hotkeys}";
        name = "Hotkeys";
      };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/standby" =
      {
        binding = "<Super>F5";
        command = "systemctl suspend";
        name = "Standby";
      };

    "org/gnome/shell/extensions/nothing-to-say" = {
      icon-visibility = "always";
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/hotkeys/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/standby/"
      ];
      area-screenshot-clip = [ "Print" ];
      screenshot = [ ];
    };
  };
}
