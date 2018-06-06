{ config, lib, pkgs, ... }:

# A temporary hack to `loginctl enable-linger $somebody` (for
# multiplexer sessions to last), until this one is unresolved:
# https://github.com/NixOS/nixpkgs/issues/3702
#
# Usage: `users.extraUsers.somebody.linger = true` or slt.

with lib;

let

  dataDir = "/var/lib/systemd/linger";

  lingeringUsers = map (u: u.name) (attrValues (flip filterAttrs config.users.users (n: u: u.linger)));

  lingeringUsersFile = builtins.toFile "lingering-users"
    (concatStrings (map (s: "${s}\n")
      (sort (a: b: a < b) lingeringUsers))); # this sorting is important for `comm` to work correctly

  updateLingering = pkgs.writeScript "update-lingering" ''
    # Stop when the system is not running, e.g. during nixos-install
    [[ -e /run/booted-system ]] || exit 0
    lingering=$(ls ${dataDir} 2> /dev/null | sort)
    echo "$lingering" | comm -3 -1 ${lingeringUsersFile} - | xargs -r ${pkgs.systemd}/bin/loginctl disable-linger
    echo "$lingering" | comm -3 -2 ${lingeringUsersFile} - | xargs -r ${pkgs.systemd}/bin/loginctl enable-linger
  '';

in

{
  options = {
    users.users = mkOption {
      options = [{
        linger = mkEnableOption "lingering for the user";
      }];
    };
  };

  config = {
    system.activationScripts.update-lingering =
      stringAfter [ "users" ] updateLingering;
  };
}
