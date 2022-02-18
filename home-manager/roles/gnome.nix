{ pkgs, lib, ... }@args:
let
  hotkeys = import ./hotkeys.nix args;
  mkTuple = lib.hm.gvariant.mkTuple;
  statusScript = pkgs.writeHaskellScript
    {
      name = "status-script";
      bins = [ pkgs.notmuch pkgs.coreutils pkgs.git pkgs.playerctl ];
      imports = [
        "Control.Exception"
        "System.Directory"
      ];
    } ''
    data Mode = Klausur | Orga | Communication | Code | Leisure | Unrestricted deriving (Eq, Ord, Show, Enum, Bounded)
    modes = enumFrom Klausur
    getMode = do
      name <- Text.strip <$> readFileText "/home/maralorn/.mode" `onException` say "File /home/maralorn/.mode not found."
      maybe (say [i|Unknown mode #{name}|] >> error [i|Unknown mode #{name}|]) pure $ find (\mode -> name == (Text.toLower $ show mode)) $ modes

    isDirty gitDir = ((/= "") <$> (git "--no-optional-locks" "-C" gitDir "status" "--porcelain" |> captureTrim)) `catch` (\(_ :: SomeException) -> pure True)
    isUnpushed gitDir = do
      revs <- LBS.split 10 <$> tryCmd (git "--no-optional-locks" "-C" gitDir "rev-parse" "@{u}" "HEAD")
      pure $ length revs /= 2 || (revs !!? 0 /= revs !!? 1)

    tryCmd x = ignoreFailure x |> captureTrim

    main = do
      playing <- Text.intercalate " " . fmap decodeUtf8 . filter (/= "") <$> mapM tryCmd [playerctl "status", playerctl "metadata" "title", playerctl "metadata" "artist"]
      mode <- getMode
      unread <- notmuch "count" "folder:hera/Inbox" "tag:unread" |> captureTrim
      inbox <- notmuch "count" "folder:hera/Inbox" |> captureTrim
      codeMails <- notmuch "count" "folder:hera/Code" |> captureTrim
      dirs <- listDirectory "/home/maralorn/git"
      dirty <- fmap toText <$> filterM (isDirty . ("/home/maralorn/git/"<>)) dirs
      unpushed <- fmap toText <$> filterM (isUnpushed . ("/home/maralorn/git/"<>)) dirs
      say . Text.intercalate " " $
        [playing, show mode] ++
        memptyIfFalse ((unread /= "0") && mode >= Orga) (one [i|Unread: #{unread}|]) ++
        memptyIfFalse ((inbox /= "0") && mode == Leisure) (one [i|Inbox: #{inbox}|]) ++
        memptyIfFalse ((codeMails /= "0") && mode == Code) (one [i|Code: #{codeMails}|]) ++
        memptyIfFalse (length unpushed /= 0) (one [i|Unpushed: #{Text.intercalate " " unpushed}|]) ++
        memptyIfFalse (length dirty /= 0) (one [i|Dirty: #{Text.intercalate " " dirty}|])
  '';
in
{
  services.gpg-agent.pinentryFlavor = "gnome3";
  dconf.settings = {
    "org/gnome/desktop/wm/keybindings" = {
      switch-input-source = [ ];
      switch-input-source-backward = [ ];
      switch-applications = [ ];
      switch-applications-backward = [ ];
      cycle-windows = [ "<Super>Tab" ];
      cycle-windows-backward = [ "<Shift><Super>Tab" ];
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
      close = [ "<Super>q" ];
    };

    "org/gnome/shell/extensions/gtile" =
      let
        left = r: "0:${r} 1:${r},0:${r} 2:${r},0:${r} 3:${r},0:${r} 0:${r}, 1:${r} 1:${r}";
        right = r: "4:${r} 5:${r},3:${r} 5:${r},2:${r} 5:${r},5:${r} 5:${r}, 4:${r} 4:${r}";
        middle = r: "2:${r} 3:${r}, 1:${r} 4:${r}, 0:${r} 5:${r}, 1:${r} 3:${r}, 2:${r} 4:${r}, 2:${r} 2:${r}, 3:${r} 3:${r}";
      in
      {
        global-presets = true;
        grid-sizes = "6x2";
        preset-resize-1 = [ "<Control><Super>m" ];
        preset-resize-2 = [ "<Control><Super>comma" ];
        preset-resize-3 = [ "<Control><Super>period" ];
        preset-resize-4 = [ "<Control><Super>n" ];
        preset-resize-5 = [ "<Control><Super>r" ];
        preset-resize-6 = [ "<Control><Super>t" ];
        preset-resize-7 = [ "<Control><Super>h" ];
        preset-resize-8 = [ "<Control><Super>g" ];
        preset-resize-9 = [ "<Control><Super>f" ];
        resize1 = "6x2 ${left "1"}";
        resize2 = "6x2 ${middle "1"}";
        resize3 = "6x2 ${right "1"}";
        resize4 = "6x1 ${left "0"}";
        resize5 = "6x1 ${middle "0"}";
        resize6 = "6x1 ${right "0"}";
        resize7 = "6x2 ${left "0"}";
        resize8 = "6x2 ${middle "0"}";
        resize9 = "6x2 ${right "0"}";
        show-toggle-tiling-alt = [ "<Super>t" ];
        show-icon = false;
      };
    # Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
    "org/gnome/desktop/a11y/keyboard" = {
      mousekeys-accel-time = 2000;
      mousekeys-enable = true;
      mousekeys-init-delay = 0;
      mousekeys-max-speed = 2000;
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
      center-commands-json = ''{"commands":[{"command":"${statusScript}/bin/status-script","interval":1,"uuid":"d20a15a4-aea9-48e1-955f-4bd9f55b08bc"}]}'';
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
      panel-element-positions = ''{"0":[{"element":"showAppsButton","visible":false,"position":"stackedTL"},{"element":"taskbar","visible":false,"position":"stackedTL"},{"element":"dateMenu","visible":true,"position":"stackedTL"},{"element":"leftBox","visible":true,"position":"stackedTL"},{"element":"activitiesButton","visible":false,"position":"stackedTL"},{"element":"centerBox","visible":true,"position":"stackedBR"},{"element":"rightBox","visible":true,"position":"stackedBR"},{"element":"systemMenu","visible":true,"position":"stackedBR"},{"element":"desktopButton","visible":false,"position":"stackedBR"}]," 0 ":[{" element ":" showAppsButton "," visible ":false," position ":" stackedTL "},{" element ":" activitiesButton "," visible ":false," position ":" stackedTL "},{" element ":" dateMenu "," visible ":true," position ":" stackedTL "},{" element ":" leftBox "," visible ":true," position ":" stackedTL "},{" element ":" taskbar "," visible ":true," position ":" stackedTL "},{" element ":" rightBox "," visible ":true," position ":" stackedBR "},{" element ":" centerBox "," visible ":true," position ":" stackedTL "},{" element ":" systemMenu "," visible ":true," position ":" stackedBR "},{" element ":" desktopButton "," visible ":true," position ":" stackedBR "}]}'';
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
        command = "kitty ${pkgs.haskell-dialog}/bin/hotkeys ${pkgs.writeText "hotkeys.yaml" (builtins.toJSON hotkeys)}";
        name = "Hotkeys";
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
      ];
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
