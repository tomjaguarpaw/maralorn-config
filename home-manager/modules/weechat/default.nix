{ lib, pkgs, config, ... }:
with lib;
let
in {

options.m-0.weechat = {
  enable = mkEnableOption "Weechat";
  channels = mkOption {
    type = types.str;
    default = "";
  };
  user = mkOption {
    type = types.str;
  };
  pw = mkOption {
    type = types.str;
  };
};

config = mkIf config.m-0.weechat.enable {
  home.file = {
    python_plugins = {
      target = ".weechat/python";
      source = ./plugins/python;
    };
    perl_plugins = {
      target = ".weechat/perl";
      source = ./plugins/perl;
    };
    plugins = {
      target = ".weechat/plugins.conf";
      text = ''
        [var]
        python.buffer_autohide.hide_inactive = on
        python.buffer_autohide.hide_private = on
      '';
    };
    weechat = {
      target = ".weechat/weechat.conf";
      text = ''
        [look]
        buffer_notify_default = "highlight"
        jump_current_to_previous_buffer = off

        [color]
        chat_nick_colors = "cyan,magenta,green,brown,lightblue,default,lightcyan,lightmagenta,lightgreen,blue,31,35,38,40,49,63,70,80,92,99,112,126,130,138,142,148,160,162,167,169,174,176,178,184,186,210,212,215,228"

        [filter]
        irc_smart = on;*;irc_smart_filter;*
      '';
    };
    logger = {
      target = ".weechat/logger.conf";
      text = ''
        [look]
        backlog = 1000

        [file]
        mask = "$name/%Y"
        path = "${config.home.homeDirectory}/data/logs/"
      '';
    };
    irc = {
      target = ".weechat/irc.conf";
      text = ''
        [look]
        color_nicks_in_nicklist = on

        [server]
        hackint.addresses = "irc.hackint.org/6697"
        hackint.ssl = on
        hackint.sasl_mechanism = plain
        hackint.sasl_username = "${config.m-0.weechat.user}"
        hackint.sasl_password = "${config.m-0.weechat.pw}"
        hackint.autoconnect = on
        hackint.username = "${config.m-0.weechat.user}"
        hackint.autojoin = "${config.m-0.weechat.channels}"
      '';
    };
  };

  systemd.user.services = {
    weechat = {
      Unit = {
        Description = "Weechat Tmux Session";
      };
      Service = {
        Type = "forking";
        ExecStart = "${pkgs.tmux}/bin/tmux -L weechat -2 new-session -d -s irc -n weechat '${pkgs.weechat}/bin/weechat'";
        Restart = "always";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
};

}
