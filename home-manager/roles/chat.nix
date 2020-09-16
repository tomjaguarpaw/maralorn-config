{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs) discord signal-desktop tdesktop dino element-desktop;
    weechat = pkgs.writeShellScriptBin "weechat" "ssh -t hera 'tmux -L weechat attach'";
  };
}
