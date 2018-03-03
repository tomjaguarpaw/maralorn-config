{ pkgs, ... }:
{
  imports = [
    ../../snippets/everywhere.nix
    ../../snippets/graphical.nix
    ../../snippets/latex.nix
    # ./syncthing.nix
    # ./nix-gc.nix
    # ./nix-update-channel.nix
  ];

  systemd.user.systemctlPath = "/usr/bin/systemctl";
  services.syncthing = {
    enable = true;
    tray = true;
  };
}
