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
  config = mkIf config.m-0.git-server.enable {
    security.sudo.extraRules = [ { commands = [ { command = "sudo nixos-rebuild switch --option tarball-ttl 0"; options = [ "SETENV" "NOPASSWD" ]; users = "git"; } ]; } ];
    services.gitolite = {
      enable = true;
      user = "git";
      adminPubkey = builtins.elemAt me.keys 0;
      extraGitoliteRc = ''
        $RC{AUTH_OPTIONS} = 'no-port-forwarding,no-X11-forwarding,no-pty';
      '';
      commonHooks = [ "${pkgs.writeShellScriptBin "post-update" ''
        if [ -n "$GL_OPTION_MIRROR" ]; then git push --all $GL_OPTION_MIRROR; fi
        if [ -n "$GL_OPTION_REBUILD" ]; then sudo nixos-rebuild switch --option tarball-ttl 0; fi
      ''}/bin/post-update" ];
    };
  };
}
