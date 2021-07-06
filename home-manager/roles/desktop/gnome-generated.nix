# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "org/gnome/desktop/wm/keybindings" = {
      switch-input-source = [ ];
      switch-input-source-backward = [ ];
    };

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-type = "nothing";
    };

    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Super>q" ];
    };

    "org/gnome/desktop/interface" = {
      gtk-im-module = "gtk-im-context-simple";
      gtk-theme = "Arc";
      icon-theme = "Arc";
    };

    "org/gnome/shell/extensions/gtile" = {
      show-toggle-tiling-alt = [ "<Super>t" ];
    };

    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [ "gTile@vibou" "clipboard-indicator@tudmotu.com" "appindicatorsupport@rgcjonas.gmail.com" "nothing-to-say@extensions.gnome.wouter.bolsterl.ee" "notification-position@drugo.dev" "drive-menu@gnome-shell-extensions.gcampax.github.com" "sound-output-device-chooser@kgshank.net" "system-monitor@paradoxxx.zero.gmail.com" "user-theme@gnome-shell-extensions.gcampax.github.com" ];
      welcome-dialog-last-shown-version = "40.1";
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

  };
}
