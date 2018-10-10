{ pkgs, config, ... }:
{

imports = [
  ../../home-manager
  ./secret
];

m-0 = {
  hostName = "hera";
  taskwarrior.enable = true;
  habitask.enable = true;
  bugwarrior.enable = true;
  rustdev.enable = true;
  update_tasks.enable = true;
  eventd.enable = true;
  weechat = {
    enable = true;
    user = config.m-0.private.hackint.user;
    pw = config.m-0.private.hackint.hackint_pw;
    channels = config.m-0.private.hackint.channels;
  };
  mail = {
    enable = true;
    boxes = with config.m-0.private.mail; [ private work work2 ];
    sendmail = with config.m-0.private.sendmail; [ private work work2 club club2 ];
    default = config.m-0.private.sendmail.private;
  };
};

}
