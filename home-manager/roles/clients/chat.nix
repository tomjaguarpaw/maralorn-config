{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      discord
      signal-desktop
      tdesktop
      element-desktop
      rocketchat-desktop
    ;
    weechat =
      pkgs.writeShellScriptBin "weechat"
        "ssh -t hera 'TMUX_TMPDIR=/run/user/1000 tmux -L weechat attach'";
  };
  xdg.configFile."Element/config.json".text = builtins.toJSON { showLabsSettings = true; };
}
