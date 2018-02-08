let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in
{
  boot.kernel.sysctl = { "fs.inotify.max_user_watches" = 204800; };
  services = {
    syncthing = {
      dataDir = "/home/maralorn/.config/syncthing";
      enable = true;
      group = "users";
      user = "maralorn";
      openDefaultPorts = true;
      useInotify = true;
      package = unstable.syncthing;
    };
  };
}
