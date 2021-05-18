{ pkgs, ... }:
{
  home.packages = builtins.attrValues rec {
    inherit (pkgs) discord signal-desktop tdesktop dino element-desktop;
    weechat = pkgs.writeShellScriptBin "weechat" "ssh -t hera 'tmux -L weechat attach'";
    chat = pkgs.writeHaskellScript
      {
        name = "chat";
        bins = [ element-desktop signal-desktop weechat discord tdesktop dino pkgs.kitty ];
      } ''
      main = mapConcurrently_ Relude.id [ element_desktop, signal_desktop, _Discord, telegram_desktop, kitty "weechat", dino ]
    '';
  };
}
