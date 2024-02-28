{
  environment.persistence.snapshoted.users.maralorn = {
    directories = [
      ".aqbanking" # Save user login
      ".aws" # Save user login
      ".cache/rbw" # Save user login
      ".calendars" # Cache synced calendars
      ".cabal/store" # Save user login
      ".config/Element" # Save user login
      ".config/Mumble" # Save known servers
      ".config/Signal" # Save user login
      ".config/discord" # Save user login
      ".config/gh" # Save user login
      ".config/heroic" # Save user login
      ".config/tea" # Save user login
      ".config/teamviewer" # Save user login
      ".config/remmina" # RDP Client
      ".contacts" # Cache synced addressbook
      ".factorio" # Factorio save games and login
      ".gnupg"
      ".local/share/Mumble"
      ".local/share/PrismLauncher"
      ".local/share/Steam"
      ".local/share/TelegramDesktop"
      ".local/share/direnv"
      ".local/share/keyrings"
      ".local/share/khal"
      ".local/share/mpd"
      ".local/share/newsboat"
      ".local/share/remmina" # RDP Client
      ".local/state/mpv" # To save watch later
      ".minecraft"
      ".mozilla/firefox"
      ".ssh"
      ".steam"
      ".task"
      ".vdirsyncer"
      ".weechat"
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
    ".cache/hie-bios"
    ".cache/ghcide"
    ".cache/nix-output-monitor"
    ".local/state/wireplumber" # For volume levels
  ];
}
