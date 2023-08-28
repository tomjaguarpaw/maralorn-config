{ pkgs, ... }:
{
  home.packages = [
    pkgs.slurp
    pkgs.grim
    (pkgs.writeShellScriptBin "screenshot" ''grim -g "$(slurp)" - | wl-copy'')
  ];
}
