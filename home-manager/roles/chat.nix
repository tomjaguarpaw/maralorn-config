{ pkgs, ... }:
{
  home.packages = builtins.attrValues rec {
    inherit (pkgs) discord signal-desktop tdesktop dino element-desktop;
    weechat = pkgs.writeShellScriptBin "weechat" "ssh -t hera 'TMUX_TMPDIR=/run/user/1000 tmux -L weechat attach'";
  };
}
