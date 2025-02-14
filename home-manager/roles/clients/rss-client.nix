{
  pkgs,
  lib,
  config,
  ...
}:
let
  rbw-unwrapped = config.programs.rbw.package;
  rbw = pkgs.writeShellScript "rbw" "PATH=${lib.makeBinPath [ rbw-unwrapped ]} rbw \"$@\"";
  dl_cmd = "${lib.getExe pkgs.yt-dlp} --restrict-filenames --trim-filenames 128 --no-part -f 'bv*[width<=1920]+ba/bv*+ba/b' --embed-subs --embed-chapters --embed-metadata --downloader ffmpeg";
  download-and-watch = pkgs.writeShellScriptBin "download-and-watch" ''
    set -euo pipefail
    video_dir="${config.home.homeDirectory}/Videos"
    cd "$video_dir"

    link="''${1:-`${lib.getExe' pkgs.wl-clipboard "wl-paste"}`}"

    play_downloaded() {
      ${lib.getExe config.programs.mpv.finalPackage} "$filename"
      echo "Enter 'y' to delete $filename:"
      read delete
      if [[ "$delete" == "y" ]] then ${lib.getExe' pkgs.coreutils "rm"} "$filename"; fi
    }

    filename="`${dl_cmd} $link --get-filename`"
    if [[ ! -f "$filename" ]]; then
      echo "'p': play directly, 'd': download, 'c': download and play"
      read command

      if [[ "$command" == "p" ]]; then ${lib.getExe config.programs.mpv.finalPackage} $link; fi
      if [[ "$command" == "d" || "$command" == "c" ]]; then
        echo "Launching prefetch …"
        # --user to use the user daemon
        # --no-block will wait for the unit to be created but not for it to signal succesful start
        # -G remove the unit immediately after exit, even if it fails
        ${lib.getBin pkgs.systemd}/bin/systemd-run --user --no-block -G \
          ${lib.getExe pkgs.kitty} -d "$video_dir" \
          /bin/sh -c \
          "${dl_cmd} \"$link\""
      fi
      if [[ "$command" == "c" ]]; then
         echo "Waiting for $filename to appear …"
         while [[ ! -f "$filename" ]]; do
           ${lib.getExe' pkgs.coreutils "sleep"} 3s;
           echo -n "."
         done
         echo " File found."
         play_downloaded
      fi
    else
      echo "File already fetched. Playing …"
      play_downloaded
    fi
  '';
  commands =
    builtins.mapAttrs
      (
        name:
        {
          config ? "",
          user,
        }:
        let
          configFile = pkgs.writeText "${name}-newsboat-config" ''
            show-read-feeds no
            show-read-articles no
            max-items 1000
            miniflux-min-items 1000
            datetime-format "%Y-%m-%d"
            urls-source "miniflux"
            miniflux-url "https://rss.maralorn.de/"
            miniflux-login "${user}"
            miniflux-passwordeval "${rbw} get rss.maralorn.de ${user}"
            ${config}
          '';
        in
        pkgs.writeShellScriptBin name ''PATH=${lib.makeBinPath [ pkgs.bash ]} ${lib.getExe pkgs.newsboat} -r -C ${configFile} -c ~/.local/share/newsboat/${name}-cache.db "$@"''
      )
      {
        news = {
          user = "maralorn";
        };
        software-updates = {
          user = "maralorn-softwareupdates";
        };
        watchfeeds = {
          user = "maralorn-watchfeeds";
          config = ''
            browser "${lib.getExe download-and-watch} %u"
          '';
        };
      }
    // {
      inherit download-and-watch;
    };
in
{
  status-script.env = [ commands.software-updates ];
  systemd.user = {
    services.update-software-feeds = {
      Unit.Description = "Update software feeds";
      Service = {
        Type = "oneshot";
        ExecStart =
          (pkgs.writeShellScript "update-software-feeds" "${commands.software-updates}/bin/software-updates -x reload")
          .outPath;
        Restart = "on-failure";
        RestartSec = "30m";
      };
      Unit.StartLimitIntervalSec = "3h";
    };
    timers.update-software-feeds = {
      Unit.Description = "Update software feeds";
      Timer.OnCalendar = "00:05";
      Install.WantedBy = [ "timers.target" ];
    };
  };
  home.packages = builtins.attrValues commands;
}
