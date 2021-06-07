{ ... }: {
  programs.firefox.enable = true;
  programs.browserpass.enable = true;
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    MOZ_DBUS_REMOTE = 1;
  };
}
