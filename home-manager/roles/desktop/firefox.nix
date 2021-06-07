{ ... }: {
  programs.firefox.enable = true;
  programs.browserpass.enable = true;
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1; # So that firefox uses wayland.

    # So that electron can open firefox links. See
    # Issue: https://github.com/electron/electron/issues/28436
    # Solution from: https://wiki.archlinux.org/title/Firefox#Applications_on_Wayland_can_not_launch_Firefox
    MOZ_DBUS_REMOTE = 1;
  };
}
