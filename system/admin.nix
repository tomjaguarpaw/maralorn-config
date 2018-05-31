{config, ...}:
let
  me = config.m-0.private.me;
in {
  users.users = {
    "${me.user}" = {
      description = me.name;
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "systemd-journal" "networkmanager" "docker" ];
      openssh.authorizedKeys.keys = me.keys;
      passwordFile = me.pw-file;
    };
    root = {
      openssh.authorizedKeys.keys = me.keys;
      passwordFile = me.pw-file;
    };
  };
}
