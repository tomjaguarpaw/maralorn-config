{
  lib,
  pkgs,
  config,
  ...
}:
let
  weechat = pkgs.wrapWeechat pkgs.weechat-unwrapped {
    configure =
      {availablePlugins, ...}:
      {
        plugins = builtins.attrValues (
          availablePlugins
          // {
            python = availablePlugins.python.withPackages (_: [pkgs.weechatScripts.weechat-matrix]);
          }
        );
        scripts = [pkgs.weechatScripts.weechat-matrix];
      };
  };
in
{
  home.file = {
    python_plugins = {
      target = ".weechat/python";
      source = ./plugins/python;
    };
    perl_plugins = {
      target = ".weechat/perl";
      source = ./plugins/perl;
    };
    buflist = {
      target = ".weechat/buflist.conf";
      text = ''
        [look]
        enabled = off
      '';
    };
    irc = {
      target = ".weechat/irc.conf";
      text = ''
        [server]
        hackint.nicks = "maralorn-irc,irclorn"
        hackint.addresses = "irc.hackint.org/6697"
        hackint.ssl = on
        hackint.ipv6 = on
      '';
    };
    weechat = {
      target = ".weechat/weechat.conf";
      text = ''
        [look]
        buffer_notify_default = "highlight"
        jump_current_to_previous_buffer = off
        highlight_words = ""
        highlight_regex = ".*maralorn([^\.].*|\.|\.[^d].*|)$"

        [key]
        meta-g = "/go"
        ${builtins.readFile ./default-keys.conf}

        [color]
        chat_nick_colors = "cyan,magenta,green,brown,lightblue,default,lightcyan,lightmagenta,lightgreen,blue,31,35,38,40,49,63,70,80,92,99,112,126,130,138,142,148,160,162,167,169,174,176,178,184,186,210,212,215,228"
        chat_highlight = "black"
        chat_highlight_bg = "lightblue"
      '';
    };
    logger = {
      target = ".weechat/logger.conf";
      text = ''
        [look]
        backlog = 10000

        [mask]
        python = "%Y/matrix:$server/$channel/%Y-%m-%d-$name.weechatlog"

        [file]
        mask = "%Y/$name/%Y-%m-%d.weechatlog"
        path = "${config.home.homeDirectory}/logs/"
      '';
    };
    matrix = {
      target = ".weechat/matrix.conf";
      text = ''
        [look]
        human_buffer_names = on
        [server]
        ${lib.concatStringsSep "\n" (
          lib.mapAttrsToList
            (server: serverConfig: ''
              ${server}.address = "${serverConfig.address}"
              ${server}.autoconnect = on
              ${server}.username = "${serverConfig.user}"
              ${server}.password = "${serverConfig.password}"
            '')
            (pkgs.privateValue {} "weechat/matrix")
        )}
      '';
    };
  };

  systemd.user = {
    timers.log2rss = {
      Timer.OnCalendar = "22:58";
      Install.WantedBy = ["timers.target"];
    };
    services = {
      log2rss = {
        Unit.Description = "weechat2rss";
        Service = {
          ExecStart = "${lib.getBin pkgs.rssfeeds}/bin/weechat2rss /var/www/rss/chats.xml";
          Type = "oneshot";
        };
      };
      weechat = {
        Unit.Description = "Weechat Tmux Session";
        Service = {
          Type = "forking";
          ExecStart = "${lib.getExe pkgs.tmux} -L weechat -2 new-session -d -s irc -n weechat '${weechat}/bin/weechat'";
          Restart = "always";
        };
        Install.WantedBy = ["default.target"];
      };
    };
  };
}
