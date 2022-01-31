{ pkgs, ... }:
let script = pkgs.runCommand "autosave.lua" { passthru.scriptName = "autosave.lua"; } ''
  mkdir -p $out/share/mpv/scripts/
  ln -s ${./autosave.lua} $out/share/mpv/scripts/autosave.lua
'';
in
{
  programs.mpv = {
    enable = true;
    config = {
      save-position-on-quit = true;
      osc = false;
    };
    scripts = [
      pkgs.mpvScripts.mpris
      pkgs.mpvScripts.thumbnail
      script
    ];
  };
}
