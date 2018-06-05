{ lib, pkgs, config, ... }:
with lib;
let
in {

options.m-0.weechat = {
  enable = mkEnableOption "Weechat";
  channels = mkOption {
    type = types.listOf types.str;
    default = [];
  };
  user = mkOption {
    type = types.str;
  };
  pw = mkOption {
    type = types.str;
  };
};

config = mkIf config.m-0.weechat.enable {
  systemd.user.services = {
    weechat = {
      Unit = {
        Description = "Weechat IRC Client (in tmux)";
      };
      Service = {
        ExecStart = "${pkgs.tmux}/bin/tmux -L weechat -2 new-window -kt irc:0 '${pkgs.weechat}/bin/weechat'";
        ExecStop = "${pkgs.tmux}/bin/tmux -L weechat kill-window -t irc:0";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
    weechat-tmux = {
      Unit = {
        Description = "Weechat Tmux Session";
      };
      Service = {
        ExecStart = "${pkgs.tmux}/bin/tmux -L weechat -2 new-window -d -s irc -n 'bash' '${pkgs.zsh}/bin/zsh'";
        ExecStop = "${pkgs.tmux}/bin/tmux -L weechat kill-window -t irc";
      };
    };
  };
};

}
