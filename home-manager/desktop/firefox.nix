{ ... }: {
  programs.firefox.enable = true;
  programs.browserpass.enable = true;
  home.sessionVariables.MOZ_ENABLE_WAYLAND = 1;
}
