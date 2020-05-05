{ pkgs, ... }:
let inherit (import ../lib) unstable;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs) signal-desktop tdesktop dino mumble;
    inherit (unstable) riot-desktop;
  };
}
