{config, lib, ...}:
with lib;
let
  me = config.m-0.private.me;
in {

boot.kernel.sysctl = { "fs.inotify.max_user_watches" = 204800; };
services = mkIf config.m-0.standalone.enable {
  syncthing = {
    dataDir = "/home/${me.user}/.config/syncthing";
    enable = true;
    group = "users";
    user = me.user;
    openDefaultPorts = true;
  };
};

}
