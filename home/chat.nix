{ pkgs, ... }:
let inherit (import ../lib) unstable;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs) signal-desktop tdesktop dino mumble;
    inherit (unstable) riot-desktop;
    weechat = pkgs.writeShellScriptBin "weechat" ''
      ssh -t hera "tmux -L weechat attach"
    '';
  };
}
