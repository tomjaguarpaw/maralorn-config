{ pkgs, ... }: {
  home.packages = builtins.attrValues {
    inherit (pkgs) signal-desktop tdesktop dino mumble riot-desktop;
    weechat = pkgs.writeShellScriptBin "weechat" ''
      ssh -t hera "tmux -L weechat attach"
    '';
  };
}
