{ pkgs, ... }:
{
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
          accent-color = "#3daee9";
          accent = "#3daee9";
          primary-color = "#00aff4";
          warning-color = "#da4453";
          alert = "#da4453";

          sidebar-color = "#31363b";
          roomlist-background-color = "#2a2e32";
          roomlist-text-color = "#b8b9ba";
          roomlist-text-secondary-color = "#808182";
          roomlist-highlights-color = "#4b4f54";
          roomlist-separator-color = "#64686b";

          timeline-background-color = "#1b1e20";
          timeline-text-color = "#fcfcfc";
          secondary-content = "#b8b9ba";
          tertiary-content = "#b8b9ba";
          timeline-text-secondary-color = "#74828f";
          timeline-highlights-color = "#232629";
          eventbubble-others-bg = "#232629";
          eventbubble-self-bg = "#223b49";
          eventbubble-selected-bg = "#2d5c76";
          reaction-row-button-selected-bg-color = "#346e8e";
        };
      }
    ];
    showLabsSettings = true;
  };
}
