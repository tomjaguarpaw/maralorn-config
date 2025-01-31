{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    nativeMessagingHosts = [ pkgs.firefoxpwa ];
  };

  home.sessionVariables = {
    BROWSER = "firefox";
    # So that electron can open firefox links. See
    # Issue: https://github.com/electron/electron/issues/28436
    # Solution from: https://wiki.archlinux.org/title/Firefox#Applications_on_Wayland_can_not_launch_Firefox
    MOZ_DBUS_REMOTE = 1;
  };
}
