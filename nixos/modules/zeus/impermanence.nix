{
  environment.persistence."/disk/persist" = {
    users.maralorn = {
      directories = [
        ".aqbanking"
        ".cache/rbw" # Save user login
        ".calendars"
        ".config/Element"
        ".config/Mumble"
        ".config/Signal"
        ".config/discord"
        ".config/gh"
        ".config/heroic" # Login data
        ".config/mpv/watch_later"
        ".config/tea"
        ".contacts"
        ".factorio" # Factorio save games and login
        ".gnupg"
        ".local/share/Mumble"
        ".local/share/PrismLauncher"
        ".local/share/Steam"
        ".local/share/TelegramDesktop"
        ".local/share/direnv"
        ".local/share/khal"
        ".local/share/mpd"
        ".local/share/newsboat"
        ".minecraft"
        ".mozilla/firefox"
        ".ssh"
        ".steam"
        ".task"
        ".vdirsyncer"
        ".zoom"
        ".zotero/zotero"
        "Games"
        "Maildir"
        "git"
        "media"
      ];
      files = [ ".chpwd-recent-dirs" ];
    };
  };
  environment.persistence."/disk/volatile" = {
    users.maralorn.directories = [
      "Videos"
      ".cache/kassandra"
      ".cache/nix-output-monitor"
      ".volatile"
    ];
  };
}
