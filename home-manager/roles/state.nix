defaultMode:
{ config, ... }:
let
  # Persistent means that files get snapshoted and kept for a month
  # Volatile means that files just lay on the disk
  # Backups are organized independently on this system
  home = config.home.homeDirectory;
  mkLine =
    type: to: from:
    "${type} ${to} - - - - ${from}";
in
{
  systemd.user.tmpfiles.rules = [
    (mkLine "f" "${home}/.mode" defaultMode)
    (mkLine "f" "${home}/.config/lazygit/state.yml" "startuppopupversion: 5")
    (mkLine "d" "${home}/.cache/mutt" "")
  ];
}
