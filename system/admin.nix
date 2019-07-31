{ config, lib, ... }:
with lib;
let me = config.m-0.private.me;
in {
  users.users = {
    "${me.user}" = {
      linger = true;
      description = me.name;
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "systemd-journal" "networkmanager" "docker" ];
      openssh.authorizedKeys.keys = me.keys;
      passwordFile = me.pw-file;
    };
    root = { passwordFile = me.pw-file; };
  };
}
