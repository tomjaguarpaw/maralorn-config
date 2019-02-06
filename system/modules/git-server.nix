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
    services.gitolite = {
      enable = true;
      user = "git";
      adminPubkey = builtins.elemAt me.keys 0;
      extraGitoliteRc = ''
        $RC{AUTH_OPTIONS} = 'no-port-forwarding,no-X11-forwarding,no-pty';
      '';
      commonHooks = [ "${pkgs.writeShellScriptBin "post-update" ''
        [ -z $GL_OPTION_MIRROR ] && exit
	git push --all $GL_OPTION_MIRROR
      ''}/bin/post-update" ];
    };
  };
}
