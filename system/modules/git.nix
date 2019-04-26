{ config, pkgs, lib, ... }:
with lib;

let

  me = config.m-0.private.me;

in
{
  options = {
    m-0.git-server.enable = mkOption {
      type = types.bool;
      default = false;
    };
  };
  config = let
    upgrade-command = "${pkgs.systemd}/bin/systemctl start nixos-upgrade.service";
  in
    mkIf config.m-0.git-server.enable {
    users.users.git.linger = true; # Frequent restarting of the systemd-user-unit leads to errors
    security.sudo.extraRules = [ { commands = [ { command = upgrade-command; options = [ "NOPASSWD" ]; } ];  users = [ "git" ]; } ];
    services.gitolite = {
      enable = true;
      user = "git";
      adminPubkey = builtins.elemAt me.keys 0;
      commonHooks = [ "${pkgs.writeShellScriptBin "post-update" ''
        if [ -n "$GL_OPTION_MIRROR" ]; then
          echo "Forwarding push to $GL_OPTION_MIRROR";
          git push --all $GL_OPTION_MIRROR;
        fi
        if [ -n "$GL_OPTION_REBUILD" ]; then
          echo "Triggering a system update … You can wait or disconnect";
          sudo ${upgrade-command};
          echo "Done";
        fi
      ''}/bin/post-update" ];
    };
  };
}
