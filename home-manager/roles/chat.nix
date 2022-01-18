{ pkgs, ... }:
{
  home.packages = builtins.attrValues
    {
      inherit (pkgs) discord signal-desktop tdesktop element-desktop;
      weechat = pkgs.writeShellScriptBin "weechat" "ssh -t hera 'TMUX_TMPDIR=/run/user/1000 tmux -L weechat attach'";
    };
  xdg.configFile."Element/config.json".text = builtins.toJSON {
    settingsDefaults.custom_themes = [
      {
        name = "My Theme";
        colors = {
          primary-color = "#000040";
          accent-color = "#4040ff";
        };
      }
    ];
    showLabsSettings = true;
  };
}
