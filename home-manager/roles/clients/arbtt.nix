{ pkgs, lib, ... }:
let
  package = pkgs.haskellPackages.arbtt;
in
{
  home.packages = [ package ];
  home.file.".arbtt/categorize.cfg".text = ''
    $idle > 60 ==> tag inactive,
    tag Program:$current.program,
    tag Title:$current.title,
    current window $title =~ m!/git/([^/]*)! ==> tag Project:$1,
    current window $program == "Signal" ==> tag Graph:signal,
    current window $program == "Element" ==> tag Graph:matrix,
    current window $program == "Discord" ==> tag Graph:discord,
    current window $program == "org_telegram_desktop" ==> tag Graph:telegram,
    current window $title =~ m!eomutt! ==> tag Graph:mail,
    current window $title =~ m!asecamp! ==> tag Graph:basecamp,
    current window $title =~ m!connect! ==> tag Graph:connect,
    current window $title =~ m!heilmann! ==> tag Graph:heilmann,
    current window $title =~ m!google! ==> tag Graph:google,
    current window $title =~ [/nixos/,/nixpkgs/] ==> tag Graph:nix,
    current window $title =~ [/reflex/,/obsidiansystems/] ==> tag Graph:reflex,
    current window $title =~ [/haskell/,/ghc-proposals/] ==> tag Graph:haskell,
    current window $title =~ /github/ ==> tag Graph:github,
    current window $title =~ m!git/config! ==> tag Graph:config,
    current window $title =~ [/git/,/jj/,/tig/] ==> tag Graph:jj,
    current window $program == "mpv" ==> tag Graph:video,
    current window $title =~ m!youtube! ==> tag Graph:video,
    current window $title =~ m!watchfeeds! ==> tag Graph:video,
    current window $program == "Chromium" ==> tag Graph:chromium,
    current window $program == "kitty" ==> tag Graph:term,
    current window $program == "firefox" ==> tag Graph:browser,
    $idle < 60 ==> tag Graph:other
  '';
  systemd.user = {
    timers.my-arbtt-capture = {
      Timer.OnCalendar = "*:*:10/20";
      Install.WantedBy = [ "timers.target" ];
    };
    services = {
      arbtt-graph = {
        Service = {
          Environment = "PATH=${
            lib.makeBinPath [
              pkgs.python3
              package
            ]
          }";
          ExecStart = pkgs.writeShellScript "serve-arbtt-graph" ''
            cd ~/git/arbtt-graph
            python3 arbtt-serve.py
          '';
        };
        Install.WantedBy = [ "default.target" ];
      };

      my-arbtt-capture.Service = {
        ExecStart =
          pkgs.writers.writeHaskell "my-arbtt-capture"
            { libraries = builtins.attrValues pkgs.myHaskellScriptPackages; }
            (
              builtins.replaceStrings
                [
                  "\"hyprctl\" -- NIX_BIN"
                  "\"jq\" -- NIX_BIN"
                  "\"arbtt-import\" -- NIX_BIN"
                ]
                [
                  "\"${lib.getExe' pkgs.hyprland "hyprctl"}\""
                  "\"${lib.getExe pkgs.jq}\""
                  "\"${lib.getExe' package "arbtt-import"}\""
                ]
                (builtins.readFile ./my-arbtt-capture.hs)
            );
        Type = "oneshot";
      };
    };

  };
}
