{ lib, pkgs, config, ... }:
with lib;
let
  weechat = pkgs.wrapWeechat pkgs.weechat-unwrapped {
    configure = { availablePlugins, ... }: {
      plugins = builtins.attrValues (availablePlugins // {
        python = (availablePlugins.python.withPackages
          (ps: [ pkgs.weechatScripts.weechat-matrix ]));
      });
      scripts = [ pkgs.weechatScripts.weechat-matrix ];
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
    buffer_autoset = {
      target = ".weechat/buffer_autoset.conf";
      text = ''
        [look]
        timer = 1000

        [buffer]
        python.maralorn.de.*.highlight_words = ""
        python.maralorn.de.*.highlight_regex = ".*maralorn([^\.].*|\.|\.[^d].*|)$"
      '';
    };
    buflist = {
      target = ".weechat/buflist.conf";
      text = ''
        [look]
        enabled = off
      '';
    };
    weechat = {
      target = ".weechat/weechat.conf";
      text = ''
        [look]
        buffer_notify_default = "highlight"
        jump_current_to_previous_buffer = off

        [key]
        meta-g = "/go"
        ${builtins.readFile ./default-keys.conf}

        [color]
        chat_nick_colors = "cyan,magenta,green,brown,lightblue,default,lightcyan,lightmagenta,lightgreen,blue,31,35,38,40,49,63,70,80,92,99,112,126,130,138,142,148,160,162,167,169,174,176,178,184,186,210,212,215,228"
        chat_highlight = "black"
        chat_highlight_bg = "lightblue"

        [filter]
        smart = on;*;irc_smart_filter,matrix_smart_filter;*
      '';
    };
    logger = {
      target = ".weechat/logger.conf";
      text = ''
        [look]
        backlog = 10000

        [mask]
        python = "%Y/matrix:$server/$channel/%Y-%m-%d-$name.weechatlog"
        irc = "%Y/irc:$server/$channel/%Y-%m-%d.weechatlog"

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
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList
          (server: serverConfig: ''
            ${server}.address = "${serverConfig.address}"
            ${server}.autoconnect = on
            ${server}.username = "${serverConfig.user}"
            ${server}.password = "${serverConfig.password}"
          '') (pkgs.privateValue { } "weechat/matrix"))}
      '';
    };
    irc = {
      target = ".weechat/irc.conf";
      text = ''
        [look]
        color_nicks_in_nicklist = on

        [server]
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList
          (server: serverConfig: ''
            ${server}.addresses = "${serverConfig.address}/${serverConfig.port}"
            ${server}.ssl = on
            ${server}.sasl_mechanism = plain
            ${server}.sasl_username = "${serverConfig.user}"
            ${server}.sasl_password = "${serverConfig.password}"
            ${server}.autoconnect = on
            ${server}.username = "${serverConfig.user}"
            ${server}.autojoin = "${serverConfig.channels}"
          '') (pkgs.privateValue { } "weechat/irc"))}
      '';
    };
  };

  systemd.user = {
    timers.log2rss = {
      Timer.OnCalendar = "19:55";
      Install.WantedBy = [ "timers.target" ];
    };
    services = {
      log2rss = {
        Unit.Description = "log2rss";
        Service = {
          ExecStart =
            "${pkgs.logfeed}/bin/log2rss /var/www/rss/chats.xml";
          Type = "oneshot";
        };
      };
      weechat = {
        Unit.Description = "Weechat Tmux Session";
        Service = {
          Type = "forking";
          ExecStart =
            "${pkgs.tmux}/bin/tmux -L weechat -2 new-session -d -s irc -n weechat '${weechat}/bin/weechat'";
          Restart = "always";
        };
        Install.WantedBy = [ "default.target" ];
      };
    };
  };

}
