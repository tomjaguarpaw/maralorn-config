{
  pkgs,
  lib,
  config,
  ...
}: let
  rbw = config.programs.rbw.package;
  download-and-watch = pkgs.writeShellScriptBin "download-and-watch" ''
    set -euo pipefail
    cd ~/.volatile/video-downloads

    link="''${1:-`${lib.getBin pkgs.wl-clipboard}/bin/wl-paste`}"

    title="`${lib.getExe pkgs.yt-dlp} -j $link | ${lib.getExe pkgs.jq} -r .title`"
    filename="`${lib.getExe pkgs.yt-dlp} -j $link | ${lib.getExe pkgs.jq} -r .filename`"
    if [[ -f "$filename" ]]; then
      ${lib.getExe pkgs.mpv} "$filename"
    else
      foot -derror ${lib.getExe pkgs.yt-dlp} $1 &
      ${lib.getExe pkgs.mpv} "$title"*.part
    fi
  '';
  commands =
    builtins.mapAttrs (name: {
      config ? "",
      user,
    }: let
      configFile = pkgs.writeText "${name}-newsboat-config" ''
        show-read-feeds no
        show-read-articles no
        datetime-format "%Y-%m-%d"
        urls-source "miniflux"
        miniflux-url "http://rss.maralorn.de/"
        miniflux-login "${user}"
        miniflux-passwordeval "${lib.getExe rbw} get rss.maralorn.de ${user}"
        ${config}
      '';
    in
      pkgs.writeShellScriptBin name "${lib.getExe pkgs.newsboat} -r -C ${configFile} -c ~/.local/share/newsboat/${name}-cache.db \"$@\"") {
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
    // {inherit download-and-watch;};
in {
  systemd.user = {
    services.update-software-feeds = {
      Unit.Description = "Update software feeds";
      Service = {
        Type = "oneshot";
        ExecStart = toString (pkgs.writeShellScript "update-plans" ''
          ${commands.software-updates}/bin/software-updates -x reload
        '');
        Restart = "on-failure";
      };
    };
    timers.update-software-feeds = {
      Unit.Description = "Update software feeds";
      Timer.OnCalendar = "00:05";
      Install.WantedBy = ["timers.target"];
    };
  };
  home = {
    packages = builtins.attrValues commands;
  };
}
