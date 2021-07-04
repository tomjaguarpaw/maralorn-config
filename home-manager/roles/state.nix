{ lib, ... }:
let
  # Persistent means that files get snapshoted and kept for a month
  # Volatile means that files just lay on the disk
  # Backups are organized independently on this system
  persistentStateDirs = [ "git" "media" "Maildir" ".ssh" ".task" ".gnupg" ".calendars" ".contacts" ".wallpapers" ".local/share/direnv" ];
  persistentStateFiles = [ ".chpwd-recent-dirs" ".zsh_history" ];
  volatileStateFiles = [ ];
  volatileStateDirs = [ ".steam" ".local/share/Steam" ];
  mkLine = type: to: from: "${type} ${to} - - - - ${from}";
  mkEntry = type: persistence: name:
    let
      target = "/disk/${persistence}/maralorn/${name}";
    in
    [ (mkLine "L+" "/home/maralorn/${name}" target) (mkLine type target "") ];
in
{
  systemd.user.tmpfiles.rules = lib.concatLists
    (
      map (mkEntry "f" "volatile") volatileStateFiles ++
        map (mkEntry "d" "volatile") volatileStateDirs ++
        map (mkEntry "f" "persist") persistentStateFiles ++
        map (mkEntry "d" "persist") persistentStateDirs
    ) ++ [
    (mkLine "L+" "/home/maralorn/.password-store" "git/password-store")
    (mkLine "f" "/home/maralorn/documents/orga/listen/checklisten/orga-pre.md" "")
    (mkLine "f" "/home/maralorn/tmp/today.md" "")
    (mkLine "f" "/home/maralorn/volatile/mode" "default")
  ];
}
