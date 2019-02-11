{ pkgs, config, ... }:
{

imports = [
  ../../home-manager
  ./secret
];

m-0 = {
  hostName = "hera";
  taskwarrior.enable = false;
  habitask.enable = false;
  bugwarrior.enable = false;
  rustdev.enable = false;
  eventd.enable = false;
  weechat = {
    enable = true;
    user = config.m-0.private.hackint.user;
    pw = config.m-0.private.hackint.hackint_pw;
    channels = config.m-0.private.hackint.channels;
  };
};

}
