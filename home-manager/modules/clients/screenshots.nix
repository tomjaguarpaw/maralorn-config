{ pkgs, ... }:
{
  home.packages = [
    pkgs.slurp
    pkgs.grim
    (pkgs.writeShellScriptBin "screenshot"
      "GRIM_DEFAULT_DIR=$XDG_PICTURES_DIR/screenshots grim -g $(slurp)"
    )
  ];
}
