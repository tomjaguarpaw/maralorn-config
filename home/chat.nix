{ pkgs, ... }:
let inherit (import ../lib) unfreePkgs;
in {
  home.packages = builtins.attrValues {
    inherit (unfreePkgs) discord;
    inherit (pkgs) signal-desktop tdesktop dino riot-desktop;
    weechat = pkgs.writeShellScriptBin "weechat" ''
      ssh -t hera "tmux -L weechat attach"
    '';
  };
}
