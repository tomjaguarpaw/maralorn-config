{pkgs, ...}: {
  home.packages = builtins.attrValues
  {
    inherit (pkgs) discord signal-desktop tdesktop element-desktop;
    weechat = pkgs.writeShellScriptBin "weechat" "ssh -t hera 'TMUX_TMPDIR=/run/user/1000 tmux -L weechat attach'";
  };
  xdg.configFile."Element/config.json".text = builtins.toJSON {
    settingDefaults.custom_themes = [
      {
        name = "My Theme";
        is_dark = false;
        colors = {
          accent-color = "#6060ff";
          accent = "#6060ff";
          primary-color = "#333399";
          warning-color = "#da4453";
          alert = "#da4453";

          sidebar-color = "#ddddff";
          roomlist-background-color = "#eeeeff";
          roomlist-text-color = "#00008a";
          roomlist-text-secondary-color = "#202099";
          roomlist-highlights-color = "#9988ee";
          roomlist-separator-color = "#8888aa";

          timeline-background-color = "#ffffff";
          timeline-text-color = "#000022";
          secondary-content = "#333399";
          tertiary-content = "#5555aa";
          timeline-text-secondary-color = "#222255";
          timeline-highlights-color = "#ffeeff";
          eventbubble-others-bg = "#eeeeff";
          eventbubble-self-bg = "#ccccff";
          eventbubble-selected-bg = "#c0c0ff";
          reaction-row-button-selected-bg-color = "#ddddff";
        };
      }
    ];
    showLabsSettings = true;
  };
}
