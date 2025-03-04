{
  pkgs,
  lib,
  config,
  ...
}:
let
  rbw-unwrapped = config.programs.rbw.package;
  rbw = pkgs.writeShellScript "rbw" "PATH=${lib.makeBinPath [ rbw-unwrapped ]} rbw \"$@\"";
  dl_cmd = "${lib.getExe pkgs.yt-dlp} --restrict-filenames --trim-filenames 128 --no-part -f 'bv*[vbr<=3000]+ba/wv*[vbr>=3000]+ba/b' --embed-subs --embed-chapters --embed-metadata";
  download-and-watch = pkgs.writeShellScriptBin "download-and-watch" ''
    set -euo pipefail
    video_dir="${config.home.homeDirectory}/Videos"
    cd "$video_dir"

    link="''${1:-`${lib.getExe' pkgs.wl-clipboard "wl-paste"}`}"

    linkfile=".$(echo "$link" | ${lib.getExe pkgs.sd} "[^a-zA-Z0-9]" "").json"

    if [[ ! -f "$linkfile" ]]; then
      ${dl_cmd} -j $link | ${lib.getExe pkgs.jq} '{filename, format_id, title, original_url, state: "pending"}'> $linkfile
    fi

    play_downloaded() {
      ${lib.getExe config.programs.mpv.finalPackage} "$filename"
      echo "Enter 'y' to delete $filename:"
      read delete
      if [[ "$delete" == "y" ]] then ${lib.getExe' pkgs.coreutils "rm"} "$filename" "$linkfile"; fi
    }

    filename="$(${lib.getExe pkgs.jq} -r .filename $linkfile)"
    formatplus="$(${lib.getExe pkgs.jq} -r .format_id $linkfile | ${lib.getExe pkgs.sd} "[0-9]" "")"

    if [[ -f "$filename" ]]; then
      command="c"
    else
      echo "'p': play directly, 'd': download, 'c': download and play"
      read command
    fi

    if [[ "$command" == "p" ]]; then ${lib.getExe config.programs.mpv.finalPackage} $link; fi
    if [[ ("$command" == "d" || "$command" == "c") && "$(${lib.getExe pkgs.jq} -r '.state' $linkfile)" != "finished" ]]; then
      echo "Launching fetch …"
      # --user to use the user daemon
      # --no-block will wait for the unit to be created but not for it to signal succesful start
      # -G remove the unit immediately after exit, even if it fails
      if [[ "+" == "$formatplus" && "$command" == "c" ]]; then
        extraargs="--downloader ffmpeg"
        echo "Optimizing for instant replay"
      else
        extraargs=""
      fi
      ${lib.getExe pkgs.jq} '.state = "finished"' $linkfile > $linkfile.finished
      ${lib.getBin pkgs.systemd}/bin/systemd-run --user --no-block -G \
        ${lib.getExe pkgs.kitty} -d "$video_dir" \
        /bin/sh -c \
        "${dl_cmd} $extraargs \"$link\" && mv $linkfile.finished $linkfile"
    fi
    if [[ "$command" == "c" ]]; then
       while [[ ! -f "$filename" ]]; do
         echo "Waiting for $filename to appear …"
         ${lib.getExe' pkgs.coreutils "sleep"} 1s;
         echo -n "."
       done
       if [[ "$(${lib.getExe pkgs.jq} -r '.state' $linkfile)" != "finished" ]]; then
         echo "Waiting 5 seconds because download is still running."
         ${lib.getExe' pkgs.coreutils "sleep"} 5s;
       fi
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
