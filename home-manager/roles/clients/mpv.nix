{pkgs, ...}:
let
  script =
    (pkgs.recursiveLinkFarm "autosave.lua" {"share/mpv/scripts/autosave.lua" = ./autosave.lua;})
    // {
      scriptName = "autosave.lua";
    };
in
{
  programs.mpv = {
    enable = true;
    config = {
      save-position-on-quit = true;
    };
    scripts = [
      pkgs.mpvScripts.mpris
      script
    ];
  };
}
