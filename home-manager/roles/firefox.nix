startpage: { pkgs, ... }: {
  programs.firefox = {
    profiles.maralorn-default = {
      extraConfig = ""; # user.js
      userChrome = ""; # css
      userContent = ""; # css
      settings = {
        "browser.search.region" = "DE";
        "distribution.searchplugins.defaultLocale" = "de-DE";
        "general.useragent.locale" = "de-DE";
        "identity.sync.tokenserver.uri" = "https://firefox-sync.maralorn.de/token/1.0/sync/1.5";
        "browser.download.useDownloadDir" = "false";
        "browser.newtab.extensionControlled" = "true";
        "browser.newtab.privateAllowed" = "true";
        "font.name.serif.x-western" = "B612";
        "services.sync.username" = "firefox@maralorn.de";
        "browser.startup.homepage" = startpage;
      };
    };
    enable = true;
  };

  programs.browserpass = {
    browsers = [ "firefox" ];
    enable = true;
  };
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1; # So that firefox uses wayland.

    # So that electron can open firefox links. See
    # Issue: https://github.com/electron/electron/issues/28436
    # Solution from: https://wiki.archlinux.org/title/Firefox#Applications_on_Wayland_can_not_launch_Firefox
    MOZ_DBUS_REMOTE = 1;
  };
}
