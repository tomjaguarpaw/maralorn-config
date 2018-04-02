{
  boot.kernel.sysctl = { "fs.inotify.max_user_watches" = 204800; };
  services = {
    syncthing = {
      dataDir = "/home/maralorn/.config/syncthing";
      enable = true;
      group = "users";
      user = "maralorn";
      openDefaultPorts = true;
    };
  };
}
