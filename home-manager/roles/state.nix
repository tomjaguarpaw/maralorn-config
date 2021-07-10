{ lib, config, ... }:
let
  # Persistent means that files get snapshoted and kept for a month
  # Volatile means that files just lay on the disk
  # Backups are organized independently on this system
  home = config.home.homeDirectory;
  persistentStateDirs = [
    ".calendars"
    ".config/Element"
    ".config/Signal"
    ".config/discord"
    ".contacts"
    ".gnupg"
    ".local/share/mpd"
    ".local/share/Mumble"
    ".local/share/TelegramDesktop"
    ".local/share/direnv"
    ".ssh"
    ".task"
    ".vdirsyncer"
    ".vimhist"
    ".zoom"
    ".zotero/zotero"
    "Maildir"
    "git"
    "media"
  ];
  persistentStateFiles = [ ".chpwd-recent-dirs" ".zsh_history" ];
  volatileStateFiles = [ ];
  volatileStateDirs = [ ".steam" ".local/share/Steam" ];
  mkLine = type: to: from: "${type} ${to} - - - - ${from}";
  mkEntry = type: persistence: name:
    let
      target = "/disk/${persistence}/maralorn/${name}";
    in
    [ (mkLine "L+" "${home}/${name}" target) (mkLine type target "") ];
in
{
  systemd.user.tmpfiles.rules = lib.concatLists
    (
      map (mkEntry "f" "volatile") volatileStateFiles ++ map (mkEntry "d" "volatile") volatileStateDirs ++ map (mkEntry "f" "persist") persistentStateFiles ++ map (mkEntry "d" "persist") persistentStateDirs
    ) ++ [
    (mkLine "L+" "${home}/.password-store" "git/password-store")
    (mkLine "f" "${home}/documents/orga/listen/checklisten/orga-pre.md" "")
    (mkLine "f" "${home}/tmp/today.md" "")
    (mkLine "L+" "${home}/.volatile" "/disk/volatile/maralorn")
    (mkLine "L+" "${home}/.config/nixpkgs/home.nix" "${home}/git/config/home.nix")
    (mkLine "L+" "${home}/.persist" "/disk/persist/maralorn")
    (mkLine "f" "/disk/volatile/maralorn/mode" "orga")
  ];
}
