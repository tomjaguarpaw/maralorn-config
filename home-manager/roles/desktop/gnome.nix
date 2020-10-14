{ pkgs, lib, ... }: {
  services.gpg-agent.pinentryFlavor = "gnome3";
  dconf.settings = {
    "org/gnome/desktop/input-sources" = {
      sources = [ (lib.hm.gvariant.mkTuple [ "xkb" "de+neo" ]) ]; # use neo
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
    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/hotkeys/"
      ];
      mic-mute = [ "<Primary><Shift>U+2113" ];
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
