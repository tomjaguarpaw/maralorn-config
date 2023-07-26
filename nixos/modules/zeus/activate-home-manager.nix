{ pkgs, ... }:
{
  systemd.services."activate-home-manager" = {
    path = [
      pkgs.nix
      pkgs.dbus
    ];
    script = ''
      if [[ -e /home/maralorn/.mode ]]; then
        MODE="$(cat /home/maralorn/.mode)"
      else
        MODE="klausur"
        echo klausur > /home/maralorn/.mode
      fi
      /disk/volatile/home/maralorn/.volatile/modes/$MODE/activate
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "maralorn";
    };
    wantedBy = [ "multi-user.target" ];
    # Try to avoid race conditions, when the user get’s logged in before activation was completed.
    before = [ "display-manager.service" ];
  };
}
