{
  pkgs,
  lib,
  config,
  ...
}: let
  rbw = config.programs.rbw.package;
  commands = builtins.mapAttrs (name: {
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
        browser "mpv %u"
      '';
    };
  };
in {
  systemd.user = {
    services.update-software-feeds = {
      Unit.Description = "Update software feeds";
      Service = {
        Type = "oneshot";
        ExecStart = toString (pkgs.writeShellScript "update-plans" ''
          ${commands.software-updates}/bin/software-updates -x reload
        '');
        Restart = "always";
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
