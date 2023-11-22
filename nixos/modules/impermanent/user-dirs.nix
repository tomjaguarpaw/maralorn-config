{
  environment.persistence.snapshoted.users.maralorn = {
    directories = [
      ".aqbanking"
      ".aws"
      ".cache/rbw" # Save user login
      ".calendars"
      ".cabal/store"
      ".config/Element"
      ".config/Mumble"
      ".config/Signal"
      ".config/discord"
      ".config/gh"
      ".config/heroic" # Login data
      ".config/mpv/watch_later"
      ".config/tea"
      ".config/remmina" # RDP Client
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
      ".local/share/remmina" # RDP Client
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
  environment.persistence.unsnapshoted.users.maralorn.directories = [
    "Videos"
    ".wine"
    ".cache/kassandra"
    ".cache/hie-bios"
    ".cache/ghcide"
    ".cache/nix-output-monitor"
    ".local/state/wireplumber" # For volume levels
  ];
}
